# Load the Shiny and leaflet libraries
library(shiny)
library(htmltools)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)
library(leaflet.extras)

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# USER INPUT REQUIRED
path_to_visual_filter_folder <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Visual_Filter_App/"
# END OF USER INPUT

# Set the working directory
setwd(path_to_visual_filter_folder)

# Get the required paths from the config file config.R
source(file.path(getwd(), "/config_visual_filter.R"))

if (upload_gps_data_from_csv) {
  
  # Upload the data from a csv file
  file_name <- "gps_data.csv"
  file_path <- "PATH/TO/CSV/FILE/"
  full_path <- paste0(file_path, file_name)
  data_for_filter <- read.csv(full_path)
  
} else {
  
  # Get the ATLAS database credentials from the config file
  harod_db_credentials <- list(
    system_name = system_name_harod,         # System name
    db_username = db_username_harod,         # username
    db_pass = db_pass_harod,                 # password
    db_host_ip = db_host_ip_harod,           # host IP address
    db_port_number = db_port_number_harod,   # port number
    db_name = db_name_harod                  # database name
  )
  
  # Upload data from the ATLAS server or sqlite
  source(paste0(getwd(), "/prepare_raw_atlas_data_for_visual_filter.R"))
  data_for_filter <- prepare_raw_atlas_data_for_visual_filter(animal_name_code = animal_name_code,
                                                              tag_number = tag_number,
                                                              start_time = start_time,
                                                              end_time = end_time,
                                                              raw_data_folder_path = raw_data_path,
                                                              atlas_db_credentials = harod_db_credentials)
  
}

# Add a 'Outliers' column with the values 0 for good points, and 1 for outliers
data_for_filter$Outliers <- 0

# Apply the baseline filter on the Speed, STD, and number of base stations
source(paste0(getwd(), "/apply_speed_std_nbs_filter.R"))
data_for_filter <- apply_speed_std_nbs_filter(data_for_filter,
                                              speed_threshold_baseline_filter,
                                              std_threshold_baseline_filter,
                                              nbs_threshold_baseline_filter)

# # add latitude and longitude columns to the data

# Keep original X and Y before converting
data_for_filter$X_original <- data_for_filter$X
data_for_filter$Y_original <- data_for_filter$Y

# Convert X and Y to an sf object in EPSG:2039
data_for_filter_sf <- st_as_sf(data_for_filter, coords = c("X", "Y"), crs = 2039)

# Transform to WGS84 (lat/lon)
data_for_filter_sf <- st_transform(data_for_filter_sf, crs = 4326)

# Extract transformed coordinates (lat/lon)
data_for_filter_sf$lat <- st_coordinates(data_for_filter_sf)[, 2]  # Latitude
data_for_filter_sf$lon <- st_coordinates(data_for_filter_sf)[, 1]  # Longitude

# Convert back to an sf object using lat/lon as geometry
data_for_filter_sf <- st_as_sf(data_for_filter_sf, coords = c("lon", "lat"), crs = 4326)

# Add back the original X and Y columns
st_geometry(data_for_filter_sf) <- "geometry"  # Ensure it remains an sf object
data_for_filter_sf$X <- data_for_filter$X_original
data_for_filter_sf$Y <- data_for_filter$Y_original

# Delete the columns X_original and Y_original
data_for_filter_sf$X_original <- NULL
data_for_filter_sf$Y_original <- NULL

# Reorder columns
data_for_filter_sf <- data_for_filter_sf[, c("TAG", "TIME", "X", "Y", "Z", "lat", "lon", "VARX", "VARY", "COVXY", 
                                             "NBS", "PENALTY", "dateTime", "DAY", "Outliers", "Speed_m_s", "STD", 
                                             "geometry")]

# Scripts for the leaflet map
source(paste0(getwd(), "/time_conversions.R"))

# Helper function to initialize the base map
initialize_atl_mapleaf <- function(MapProvider = map_provider, tile_opacity = 0.8) {
  leaflet() %>%
    addProviderTiles(MapProvider, options = providerTileOptions(opacity = tile_opacity)) %>%
    addDrawToolbar(
      targetGroup = "drawn_polygon",
      polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fillOpacity = 0.2)),
      circleOptions = FALSE, # Disable circle drawing
      rectangleOptions = FALSE, # Disable rectangle drawing
      markerOptions = FALSE, # Disable marker drawing
      circleMarkerOptions = FALSE, # Disable circle marker drawing
      polylineOptions = FALSE, # Disable polyline drawing
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
    ) %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE, maxWidth = 200))
}

# Helper function to update the map with data
update_atl_mapleaf <- function(proxy, dd_sf, 
                               display_non_filtered_track, 
                               zoom_flag = TRUE, 
                               color_outliers = "#E66100") {

  # Ensure that the required columns are present in the dataset
  if (!all(c("lon", "lat", "TIME", "TAG", "Outliers") %in% colnames(dd_sf))) {
    stop("Data must contain lon, lat, TIME, TAG, and Outliers columns")
  }
  
  # # Define the colors for valid points (purple) and outliers (yellow)
  color_valid_points <- "#5D3A9B"
  
  # Filter out the outliers (non-outliers will be used to create lines)
  dd_non_outliers_sf <- dd_sf %>% filter(Outliers == 0)
  dd_outliers_sf <- dd_sf %>% filter(Outliers == 1)

  # Create dateTimeFormatted from TIME column if not already present
  if (!"dateTimeFormatted" %in% colnames(dd_sf)) {
    dd_sf <- dd_sf %>%
      mutate(dateTimeFormatted = unix_timestamp_to_human_date(TIME))  # Ensure conversion happens
  }
  
  if (!"dateTimeFormatted" %in% colnames(dd_non_outliers_sf)) {
    dd_non_outliers_sf <- dd_non_outliers_sf %>%
      mutate(dateTimeFormatted = unix_timestamp_to_human_date(TIME))  # Ensure conversion happens
  }
  
  if (!"dateTimeFormatted" %in% colnames(dd_outliers_sf)) {
    dd_outliers_sf <- dd_outliers_sf %>%
      mutate(dateTimeFormatted = unix_timestamp_to_human_date(TIME))  # Ensure conversion happens
  }
  
  # Ensure that dd_sf has data and calculate the bounding box safely
  if (nrow(dd_sf) > 0) {
    bbox <- st_bbox(dd_sf)
    bbox_values <- as.numeric(c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]))
  } else {
    # If data is empty, set a default bounding box
    bbox_values <- c(-180, -90, 180, 90)  # World bounding box as a fallback
  }
  
  if (display_non_filtered_track) {
    
    # Normalize the TIME column
    time_min <- min(dd_sf$TIME, na.rm = TRUE)
    time_max <- max(dd_sf$TIME, na.rm = TRUE)
    
    # Avoid division by zero if all times are the same
    if (time_min == time_max) {
      time_normalized <- rep(0.5, nrow(dd_sf))  # All points have the same time
    } else {
      time_normalized <- (dd_sf$TIME - time_min) / (time_max - time_min)
    }
    
    # Define a custom color gradient from color1 to color2- color-blind friendly
    color1 <- "#FFC20A"
    color2 <- "#0C7BDC"
    line_color <- "#0C7BDC"
    
    # Create a color function from color1 to color2
    color_gradient <- colorRampPalette(c(color1, color2))
    
    # Assign colors to each row based on the normalized time
    dd_sf <- dd_sf %>%
      mutate(color = color_gradient(100)[as.numeric(cut(time_normalized, breaks = 100))])
    
    # Create line segments between consecutive points
    llpd_lines <- dd_sf %>%
      group_by(TAG) %>%
      summarize(do_union = FALSE) %>%
      st_cast("LINESTRING")
    
    # Generate the map
    proxy %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%  # Clear existing controls (including the legend)
    
      # Add the location points with gradient colors
      addCircleMarkers(data = dd_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = ~color, radius = 4,
                       # label = ~htmlEscape(paste0(dateTimeFormatted)),
                       label = ~htmlEscape(paste0("DateTime = ", dateTimeFormatted,
                                                  " ; STD = ", round(STD, 1),
                                                  " ; NBS = ", NBS)),
                       labelOptions = labelOptions(
                         direction = "auto",
                         opacity = 0.9,
                         offset = c(10, 10),
                         style = list(
                           "background-color" = "white",
                           "border" = "1px solid black",
                           "padding" = "3px",
                           "border-radius" = "3px"
                         )
                       )
      ) %>%
      
      # Add lines connecting the locations
      addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = line_color) %>%
      
      # Add a legend to the map
      addLegend(
        position = "topright",
        colors = c(color1, color2),
        labels = c("Start Time", "End Time"),
        title = "Unfiltered Mode",
        opacity = 1
      ) %>%
      
      # Set the map's view to fit the bounds of the data only if necessary
      {
        if (zoom_flag) {
          # Pass both corners (lat1, lng1) and (lat2, lng2) to fitBounds
          proxy %>% fitBounds(bbox_values[1], bbox_values[2], bbox_values[3], bbox_values[4])
        }
      }
    
  } else {
    
    # Create LINESTRING for connecting non-outliers points by tag
    llpd_lines <- dd_non_outliers_sf %>%
      group_by(TAG) %>%
      summarize(do_union = FALSE) %>%
      st_cast("LINESTRING")
    
    # If there are no outliers in the data set, add only the valid points
    if (!nrow(dd_outliers_sf) > 0) {
      proxy %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%  # Clear existing controls (including the legend)
        
        # Add non-outliers
        addCircleMarkers(data = dd_non_outliers_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_valid_points, radius = 4,
                         # label = ~htmlEscape(paste0(dateTimeFormatted)),
                         label = ~htmlEscape(paste0("DateTime = ", dateTimeFormatted,
                                                    " ; STD = ", round(STD, 1),
                                                    " ; NBS = ", NBS)),
                         labelOptions = labelOptions(
                           direction = "auto",
                           opacity = 0.9,
                           offset = c(10, 10),
                           style = list(
                             "background-color" = "white",
                             "border" = "1px solid black",
                             "padding" = "3px",
                             "border-radius" = "3px"
                           )
                         )
        ) %>%
        
        # Add lines connecting non-outliers
        addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = color_valid_points) %>%
        
        # Add a legend to the map
        addLegend(
          position = "topright",
          colors = c(color_valid_points, color_outliers),
          labels = c("Valid Points", "Outliers"),
          title = "Filtered Mode",
          opacity = 1
        ) %>%
        
        # Set the map's view to fit the bounds of the data only if necessary
        {
          if (zoom_flag) {
            # Pass both corners (lat1, lng1) and (lat2, lng2) to fitBounds
            proxy %>% fitBounds(bbox_values[1], bbox_values[2], bbox_values[3], bbox_values[4])
          }
        }
    } else if (!nrow(dd_non_outliers_sf) > 0) {
      # If there are no valid points in the data set, add only the outliers
      proxy %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%  # Clear existing controls (including the legend)
        
        # Add outliers with yellow color
        addCircleMarkers(data = dd_outliers_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_outliers, radius=4,
                         # label = ~htmlEscape(paste0(dateTimeFormatted)),
                         label = ~htmlEscape(paste0("DateTime = ", dateTimeFormatted,
                                                    " ; STD = ", round(STD, 1),
                                                    " ; NBS = ", NBS)),
                         labelOptions = labelOptions(
                           direction = "auto",
                           opacity = 0.9,
                           offset = c(10, 10),
                           style = list(
                             "background-color" = "white",
                             "border" = "1px solid black",
                             "padding" = "3px",
                             "border-radius" = "3px"
                           )
                         )
        ) %>%
        
        # Add a legend to the map
        addLegend(
          position = "topright",
          colors = c(color_valid_points, color_outliers),
          labels = c("Valid Points", "Outliers"),
          title = "Filtered Mode",
          opacity = 1
        ) %>%
        
        # Set the map's view to fit the bounds of the data only if necessary
        {
          if (zoom_flag) {
            # Pass both corners (lat1, lng1) and (lat2, lng2) to fitBounds
            proxy %>% fitBounds(bbox_values[1], bbox_values[2], bbox_values[3], bbox_values[4])
          }
        }
    } else {
      # Add the outliers and valid points to the map- complete proxy function
      proxy %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%  # Clear existing controls (including the legend)
        
        # Add outliers with yellow color
        addCircleMarkers(data = dd_outliers_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_outliers, radius=4,
                         # label = ~htmlEscape(paste0(dateTimeFormatted)),
                         label = ~htmlEscape(paste0("DateTime = ", dateTimeFormatted,
                                                    " ; STD = ", round(STD, 1),
                                                    " ; NBS = ", NBS)),
                         labelOptions = labelOptions(
                           direction = "auto",
                           opacity = 0.9,
                           offset = c(10, 10),
                           style = list(
                             "background-color" = "white",
                             "border" = "1px solid black",
                             "padding" = "3px",
                             "border-radius" = "3px"
                           )
                         )
        ) %>%
        
        # Add non-outliers with gradient colors
        # addCircleMarkers(data = dd_non_outliers_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = ~color, radius = 4,
        addCircleMarkers(data = dd_non_outliers_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_valid_points, radius = 4,
                         # label = ~htmlEscape(paste0(dateTimeFormatted)),
                         label = ~htmlEscape(paste0("DateTime = ", dateTimeFormatted,
                                                    " ; STD = ", round(STD, 1),
                                                    " ; NBS = ", NBS)),
                         labelOptions = labelOptions(
                           direction = "auto",
                           opacity = 0.9,
                           offset = c(10, 10),
                           style = list(
                             "background-color" = "white",
                             "border" = "1px solid black",
                             "padding" = "3px",
                             "border-radius" = "3px"
                           )
                         )
        ) %>%
        
        # Add lines connecting non-outliers
        addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = color_valid_points) %>%
        
        # Add a legend to the map
        addLegend(
          position = "topright",
          colors = c(color_valid_points, color_outliers),
          labels = c("Valid Points", "Outliers"),
          title = "Filtered Mode",
          opacity = 1
        ) %>%
        
        # Set the map's view to fit the bounds of the data only if necessary
        {
          if (zoom_flag) {
            # Pass both corners (lat1, lng1) and (lat2, lng2) to fitBounds
            proxy %>% fitBounds(bbox_values[1], bbox_values[2], bbox_values[3], bbox_values[4])
          }
        }
    }
  }
}

save_filtered_data <- function(tag_number, start_time, end_time, 
                               filtered_data_path, segment_data,
                               save_as_csv = FALSE) {
  
  # Create the file name and full path to save the filtered data
  full_path_filtered_data <- create_sqlite_filepath(animal_name_code,
                                                    tag_number, 
                                                    start_time, 
                                                    end_time, 
                                                    filtered_data_path)
  
  # Add _filtered to the file name
  # Find the position of the last dot (before the extension)
  pos <- regexpr("\\.sqlite$", full_path_filtered_data)
  
  # If a dot followed by 'sqlite' is found, insert '_raw' before it
  if (pos > 0) {
    full_path_filtered_data <- paste0(substr(full_path_filtered_data, 1, pos - 1), "_filtered", substr(full_path_filtered_data, pos, nchar(full_path_filtered_data)))
  }
  
  # Save the current data segment as sqlite
  source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))
  save_ATLAS_data_to_sqlite(localizations_data = segment_data,
                            fullpath = full_path_filtered_data)
  
  if (save_as_csv) {
    full_path_filtered_data_csv <- sub("\\.sqlite$", ".csv", full_path_filtered_data)
    write.csv(segment_data, full_path_filtered_data_csv, row.names = FALSE)
  }
  
}

move_to_next_segment <- function(data_segment_action, current_segment_index, validate_data_for_days, reactive_num_points, data_for_filter_sf) {
  if (data_segment_action == "days") {
    # Increment the current day number
    next_day_number <- current_segment_index() + 1
    day_numbers <- validate_data_for_days()  # Retrieve day numbers from the reactive
    if (next_day_number <= length(day_numbers)) {
      current_segment_index(next_day_number)
    }
  } else if (data_segment_action == "number_of_points") {
    num_points <- reactive_num_points()  # Number of points for the next segment
    start_point <- current_segment_index()  # Get the current start point
    # Calculate the new start point for the next segment
    new_start_point <- start_point + num_points
    # Ensure that we don't exceed the total number of points
    if (new_start_point <= nrow(data_for_filter_sf)) {
      current_segment_index(new_start_point)
    }
  }
}

move_to_previous_segment <- function(data_segment_action, current_segment_index, validate_data_for_days, reactive_num_points, data_for_filter_sf) {
  if (data_segment_action == "days") {
    # Decrement the current day number
    previous_day_number <- current_segment_index() - 1
    if (previous_day_number >= 1) {
      current_segment_index(previous_day_number)
    }
  } else if (data_segment_action == "number_of_points") {
    num_points <- reactive_num_points()
    # Get the current start point for the previous segment
    start_point <- current_segment_index()
    # Calculate the new start point for the previous segment
    new_start_point <- max(1, start_point - num_points)  # Prevent going below 1
    current_segment_index(new_start_point)
  }
}

# Define User Interface for the application
ui <- fluidPage(
  
  titlePanel("Visual Filter: ATLAS Data"),
  
  sidebarLayout(
    sidebarPanel(
      # Display the segment information dynamically
      p(textOutput("tag_display")),
      p(textOutput("start_time_display")),
      p(textOutput("end_time_display")),
      h2(textOutput("segment_display")),
      h3("Actions"),
      # Data segment type
      radioButtons(
        "data_segment_action", 
        label = "Segment Data by:", 
        choices = c(
          "Days" = "days", 
          "Number of Points" = "number_of_points"
        ),
        selected = "days"
      ),
      # Conditional input for number of points
      conditionalPanel(
        condition = "input.data_segment_action == 'number_of_points'",
        numericInput(
          "num_points", 
          label = "Enter Number of Points:", 
          value = 2000, 
          min = 0
        )
      ),
      # Display the Filtered track vs. Unfiltered track
      radioButtons(
        "track_display_mode",
        label = "Track Display Mode:",
        choices = c(
          "Show Filtered Data" = "filtered",
          "Show Unfiltered Data" = "unfiltered"
        ),
        selected = "unfiltered"
      ),
      # Polygon selection
      radioButtons(
        "polygon_action", 
        label = "Polygon Action:", 
        choices = c(
          "Mark as Valid Points" = "mark_valid", 
          "Mark as Outliers" = "mark_invalid"
        ),
        selected = "mark_invalid"
      ),
      actionButton("save_data", "Save Data"),
      actionButton("next_segment", "Next Segment"),
      actionButton("previous_segment", "Previous Segment")
    ),
    
    mainPanel(
      leafletOutput("map", width = "100%", height = 600)
    )
  )
)

# Define server logic for the application
server <- function(input, output, session) {
  
  # Validate that the data contains tag numbers
  tag_numbers_in_data <- as.numeric(unique(data_for_filter_sf$TAG))
  if(is.null(tag_numbers_in_data)) 
  {Er <- simpleError("No TAG numbers were found in the provided data.")
  stop(Er)}
  
  # Return an error in case the data includes more than a single tag number
  if(length(tag_numbers_in_data)>1) 
  {Er <- simpleWarning("Please use data from a single tag number. This app handles data from one tag at a time.")
  stop(Er)}
  
  # Validate that the data contains a DAY column
  validate_data_for_days <- reactive({
    if (input$data_segment_action == "days") {
      day_numbers <- unique(data_for_filter_sf$DAY)
      if (is.null(day_numbers) || length(day_numbers) == 0) {
        stop("No DAY numbers were found in the provided data.")
      }
      return(day_numbers)
    } else {
      NULL
    }
  })
  
  # Validate that the data set is not empty
  validate_data_for_points <- reactive({
    if (input$data_segment_action == "number_of_points") {
      if (nrow(data_for_filter_sf) == 0) {
        stop("No data points found in the provided data.")
      }
    }
  })
  
  # Combine validations
  observe({
    validate_data_for_days()
    validate_data_for_points()
  })
  
  # Render the initial map
  output$map <- renderLeaflet({
    initialize_atl_mapleaf()
  })
  
  # Reactive value for the current segment index
  current_segment_index <- reactiveVal()
  
  # Reactive expression to monitor changes in the number of points
  reactive_num_points <- reactive({
    input$num_points
  })
  
  # initiate the segment indices
  observeEvent(input$data_segment_action, {
    if (input$data_segment_action == "days") {
      day_numbers <- validate_data_for_days() # Retrieve day numbers from the reactive
      current_segment_index(day_numbers[1])   # Set the first day number as the starting segment
    } else if (input$data_segment_action == "number_of_points") {
      current_segment_index(1)                # Start with segment 1 for points
    }
  })
  
  # Reactive calculation for the current segment (day or points)
  observeEvent(list(current_segment_index(), input$num_points), {
    if (input$data_segment_action == "days") {
      day_numbers <- validate_data_for_days()
      day_numbers[current_segment_index()]
    } else if (input$data_segment_action == "number_of_points") {
      num_points <- reactive_num_points()
      start <- current_segment_index()
      end <- min(start + num_points - 1, nrow(data_for_filter_sf))
      list(start = start, end = end)
    }
  })
  
  # Declare segment_data as a global reactive value
  segment_data <- reactiveValues(data = NULL)
  
  # Reactive calculation for segment_data
  observeEvent(list(current_segment_index(), input$num_points), {
    if (input$data_segment_action == "days") {
      segment_data$data <- data_for_filter_sf[data_for_filter_sf$DAY == current_segment_index(), ]
    } else if (input$data_segment_action == "number_of_points") {
      num_points <- reactive_num_points()
      start <- current_segment_index()
      end <- min(start + num_points - 1, nrow(data_for_filter_sf))
      segment_range <- list(start = start, end = end)
      segment_data$data <- data_for_filter_sf[segment_range$start:segment_range$end, ]
    }
  })
  
  # Get the start and end times of the current segment
  
  # Initialize reactive values for start_time and end_time
  start_time_current_segment <- reactiveVal(NULL)
  end_time_current_segment <- reactiveVal(NULL)
  
  observe({
    
    data <- segment_data$data
    
    # Convert Unix timestamps to POSIXct
    segment_posixct_time <- as.POSIXct(data$TIME / 1000, origin = "1970-01-01", tz = atlas_time_zone)
    
    # Find and convert start and end times to human-readable format
    start_time_human_readable <- format(as.POSIXct(min(data$TIME, na.rm = TRUE) / 1000, origin = "1970-01-01", tz = atlas_time_zone), "%Y-%m-%d %H:%M:%S")
    end_time_human_readable <- format(as.POSIXct(max(data$TIME, na.rm = TRUE) / 1000, origin = "1970-01-01", tz = atlas_time_zone), "%Y-%m-%d %H:%M:%S")
    
    # Update the reactive values with human-readable times
    start_time_current_segment(start_time_human_readable)
    end_time_current_segment(end_time_human_readable)
    
  })

  
  # Display the tag number and dates of the raw data
  output$tag_display <- renderText({
    paste("Tag:", tag_number)
  })
  
  output$start_time_display <- renderText({
    paste("Start Time:", start_time_current_segment())
  })
  
  output$end_time_display <- renderText({
    paste("End Time:", end_time_current_segment())
  })
  
  # Update the map and segment_data when the current segment changes
  observeEvent(list(current_segment_index(), input$num_points), {
    if (input$data_segment_action == "days") {
      # Filter data for the current day
      segment_data$data <- data_for_filter_sf[data_for_filter_sf$DAY == current_segment_index(), ]
    } else if (input$data_segment_action == "number_of_points") {
      num_points <- reactive_num_points()
      # Calculate the start and end rows for the current segment
      start_row <- current_segment_index() 
      end_row <- min(start_row + num_points - 1, nrow(data_for_filter_sf))
      # Filter data for the current range of points
      segment_data$data <- data_for_filter_sf[start_row:end_row, ]
    }
    
    # Update the map with the new data
    leafletProxy("map") %>%
      update_atl_mapleaf(segment_data$data, display_non_filtered_track = TRUE)
  })
  
  # Display the current segment in the UI
  output$segment_display <- renderText({
    if (input$data_segment_action == "days") {
      paste("Day", current_segment_index())
    } else if (input$data_segment_action == "number_of_points") {
      num_points <- reactive_num_points()
      # Calculate the start and end points for the current segment
      start_row <- current_segment_index()
      end_row <- min(start_row + num_points - 1, nrow(data_for_filter_sf))
      paste("Points", start_row, "-", end_row)
    }
  })
  
  # Select if to display the unfiltered track or filtered track
  observe({
    if (input$track_display_mode == "filtered") {
      # Use the filtered dataset
      display_unfiltered_track = FALSE
      
    } else {
      # Use the unfiltered dataset
      display_unfiltered_track = TRUE
    }
    
    # Refresh the map
    leafletProxy("map") %>%
      update_atl_mapleaf(segment_data$data,
                         display_non_filtered_track = display_unfiltered_track,
                         zoom_flag = FALSE)
    
  })

  # Toggle a point when clicked
  observeEvent(input$map_marker_click, {

    if (input$track_display_mode == "filtered") {
      clicked_timestamp <- input$map_marker_click$id # This is now the timestamp of the clicked marker
  
      # Find the index of the clicked point
      current_data <- segment_data$data
      index <- which(current_data$TIME == as.numeric(clicked_timestamp))
  
      if (length(index) == 1) { # Ensure only one point matches
        # Toggle Outliers for the clicked point
        current_data$Outliers[index] <- ifelse(current_data$Outliers[index] == 0, 1, 0)
  
        segment_data$data <- current_data
  
        leafletProxy("map") %>%
          update_atl_mapleaf(segment_data$data,
                             display_non_filtered_track = FALSE,
                             zoom_flag = FALSE)
      }
    } else {
      showModal(
        modalDialog(
          title = "Error",
          "Points can only be toggled when the filtered data is displayed.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })
  
  # Select a polygon
  observeEvent(input$map_draw_new_feature, {
    
    if (input$track_display_mode == "filtered") {
      # Extract the drawn polygon
      feature <- input$map_draw_new_feature
      
      polygon_coords <- feature$geometry$coordinates[[1]]
      
      # Convert to sf polygon
      polygon_sf <- st_polygon(list(matrix(unlist(polygon_coords), ncol = 2, byrow = TRUE)))
      polygon_sf <- st_sfc(polygon_sf, crs = 4326)
      
      # Filter points inside the polygon, and mark them as Outliers = 1
      updated_data <- segment_data$data
      points_inside <- st_within(updated_data, polygon_sf, sparse = FALSE)
      
      # Determine action based on selected radio button
      if (input$polygon_action == "mark_invalid") {
        updated_data$Outliers[points_inside] <- 1
      } else if (input$polygon_action == "mark_valid") {
        updated_data$Outliers[points_inside] <- 0
      }
      # Update the reactive data variable
      segment_data$data <- updated_data
      
      # Refresh the map
      leafletProxy("map") %>%
        update_atl_mapleaf(segment_data$data,
                           display_non_filtered_track = FALSE,
                           zoom_flag = FALSE)
    } else {
      showModal(
        modalDialog(
          title = "Error",
          "Toggling points by a polygon is only enabled when the filtered data is displayed.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })
  
  # Save the filtered data
  observeEvent(input$save_data, {
    if (input$track_display_mode == "filtered") {
      save_filtered_data(tag_number = tag_number,
                         start_time = start_time_current_segment(),
                         end_time = end_time_current_segment(),
                         filtered_data_path = filtered_data_path,
                         segment_data = segment_data$data,
                         save_as_csv = save_filtered_data_as_csv)
      
      # Refresh the map
      leafletProxy("map") %>%
        update_atl_mapleaf(segment_data$data, 
                           display_non_filtered_track = FALSE,
                           zoom_flag = FALSE, 
                           color_outliers = "#D3D3D3")
    } else {
      showModal(
        modalDialog(
          title = "Error",
          "Data can only be saved when the filtered data is displayed.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }

  })
  
  # Navigate to the next segment
  observeEvent(input$next_segment, {
    if (input$track_display_mode == "filtered") {
      showModal(
        modalDialog(
          title = "Save Data?",
          "Do you want to save the current data before moving to the next segment?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_save_next", "Yes, Save and move to next segment"),
            actionButton("skip_save_next", "No, Don't Save and move to next segment")
          )
        )
      )
    } else {
      # Move to the next segment
      move_to_next_segment(
        data_segment_action = input$data_segment_action,
        current_segment_index = current_segment_index,
        validate_data_for_days = validate_data_for_days,
        reactive_num_points = reactive_num_points,
        data_for_filter_sf = data_for_filter_sf
      )
    }
  })
  
  # Handle "Yes, Save and move to the next segment"
  observeEvent(input$confirm_save_next, {
    # Remove the popup window
    removeModal()
    # Save the filtered data
    save_filtered_data(tag_number = tag_number,
                       start_time = start_time_current_segment(),
                       end_time = end_time_current_segment(),
                       filtered_data_path = filtered_data_path,
                       segment_data = segment_data$data,
                       save_as_csv = save_filtered_data_as_csv)
    
    # Move to the next segment
    move_to_next_segment(
      data_segment_action = input$data_segment_action,
      current_segment_index = current_segment_index,
      validate_data_for_days = validate_data_for_days,
      reactive_num_points = reactive_num_points,
      data_for_filter_sf = data_for_filter_sf
    )
    
  })
  
  # Handle "No, Don't Save and move to the next segment"
  observeEvent(input$skip_save_next, {
    removeModal()
    move_to_next_segment(
      data_segment_action = input$data_segment_action,
      current_segment_index = current_segment_index,
      validate_data_for_days = validate_data_for_days,
      reactive_num_points = reactive_num_points,
      data_for_filter_sf = data_for_filter_sf
    )
  })
  
  # Navigate to the previous segment
  observeEvent(input$previous_segment, {
    if (input$track_display_mode == "filtered") {
      showModal(
        modalDialog(
          title = "Save Data?",
          "Do you want to save the current data before moving to the previous segment?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_save_previous", "Yes, Save and move to previous segment"),
            actionButton("skip_save_previous", "No, Don't Save and move to previous segment")
          )
        )
      )
    } else {
      # Move to the previous segment
      move_to_previous_segment(
        data_segment_action = input$data_segment_action,
        current_segment_index = current_segment_index,
        validate_data_for_days = validate_data_for_days,
        reactive_num_points = reactive_num_points,
        data_for_filter_sf = data_for_filter_sf
      )
    }
  })
  
  # Handle "Yes, Save and move to the previous segment"
  observeEvent(input$confirm_save_previous, {
    # Remove the popup window
    removeModal()
    # Save the filtered data
    save_filtered_data(tag_number = tag_number,
                       start_time = start_time_current_segment(),
                       end_time = end_time_current_segment(),
                       filtered_data_path = filtered_data_path,
                       segment_data = segment_data$data,
                       save_as_csv = save_filtered_data_as_csv)
    
    # Move to the previous segment
    move_to_previous_segment(
      data_segment_action = input$data_segment_action,
      current_segment_index = current_segment_index,
      validate_data_for_days = validate_data_for_days,
      reactive_num_points = reactive_num_points,
      data_for_filter_sf = data_for_filter_sf
    )
  })
  
  # Handle "No, Don't Save and move to the previous segment"
  observeEvent(input$skip_save_previous, {
    removeModal()
    move_to_previous_segment(
      data_segment_action = input$data_segment_action,
      current_segment_index = current_segment_index,
      validate_data_for_days = validate_data_for_days,
      reactive_num_points = reactive_num_points,
      data_for_filter_sf = data_for_filter_sf
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)