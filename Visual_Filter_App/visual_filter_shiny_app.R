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

# ### USER INPUT REQUIRED
path_to_visual_filter_folder <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Visual_Filter_App/"
# ### END OF USER INPUT

# Set the working directory
setwd(path_to_visual_filter_folder)

# Get the required paths from the config file config.R
source(file.path(getwd(), "/config_visual_filter.R"))

# Source helper functions for the app
source(paste0(getwd(), "/time_conversions.R"))
source(paste0(getwd(), "/initialize_atl_mapleaf.R"))
source(paste0(getwd(), "/update_atl_mapleaf.R"))
source(paste0(getwd(), "/save_filtered_data.R"))
source(paste0(getwd(), "/move_to_next_segment.R"))
source(paste0(getwd(), "/move_to_previous_segment.R"))

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
          "Annotation Mode" = "annotation",
          "Exploratory Mode" = "exploratory"
        ),
        selected = "exploratory"
      ),
      # Polygon selection
      radioButtons(
        "annotation_action", 
        label = "Annotate Data Points:", 
        choices = c(
          "Mark as Valid Points" = "mark_valid",
          "Mark as Uncertain" = "mark_uncertain",
          "Mark as Outliers" = "mark_invalid"
        ),
        selected = "mark_invalid"
      ),
      actionButton("undo_button", label = "Undo Last Action", icon = icon("undo")),
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
      day_numbers <- validate_data_for_days()
      segment_data$data <- data_for_filter_sf[data_for_filter_sf$DAY == day_numbers[current_segment_index()], ]
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
      day_numbers <- validate_data_for_days()
      # Filter data for the current day
      segment_data$data <- data_for_filter_sf[data_for_filter_sf$DAY == day_numbers[current_segment_index()], ]
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
      day_numbers <- validate_data_for_days()
      paste("Day", day_numbers[current_segment_index()])
    } else if (input$data_segment_action == "number_of_points") {
      num_points <- reactive_num_points()
      # Calculate the start and end points for the current segment
      start_row <- current_segment_index()
      end_row <- min(start_row + num_points - 1, nrow(data_for_filter_sf))
      paste("Points", start_row, "-", end_row)
    }
  })
  
  # Select if to display the raw (exploratory) track or annotated track
  observe({
    if (input$track_display_mode == "annotation") {
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
  
  # Reactive value to store the previous state for undo
  segment_data$previous_data <- NULL

  # Toggle a point between Valid, Outlier and Uncertain when left-clicked
  observeEvent(input$map_marker_click, {

    if (input$track_display_mode == "annotation") {
      clicked_timestamp <- input$map_marker_click$id # This is now the timestamp of the clicked marker
  
      # Find the index of the clicked point
      current_data <- segment_data$data
      index <- which(current_data$TIME == as.numeric(clicked_timestamp))
  
      if (length(index) == 1) { # Ensure only one point matches
        
        # Save current state before modifying for undo functionality
        segment_data$previous_data <- current_data
        
        # Toggle the point according to the chosed Annotation radio button
        if (input$annotation_action == "mark_invalid") {
          current_data$Outliers[index] <- 1
        } else if (input$annotation_action == "mark_uncertain") {
          current_data$Outliers[index] <- 2
        } else if (input$annotation_action == "mark_valid") {
          current_data$Outliers[index] <- 0
        }
        
        # # Toggle Outliers column in cyclic order: 0 → 1 → 2 → 0
        # current_data$Outliers[index] <- (current_data$Outliers[index] + 1) %% 3
        
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
          "Points can only be toggled in the Annotation Mode.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })
  
  # Select a polygon
  observeEvent(input$map_draw_new_feature, {
    
    if (input$track_display_mode == "annotation") {
      # Extract the drawn polygon
      feature <- input$map_draw_new_feature
      
      polygon_coords <- feature$geometry$coordinates[[1]]
      
      # Convert to sf polygon
      polygon_sf <- st_polygon(list(matrix(unlist(polygon_coords), ncol = 2, byrow = TRUE)))
      polygon_sf <- st_sfc(polygon_sf, crs = 4326)
      
      # Extract the points that are inside the marked polygon
      updated_data <- segment_data$data
      points_inside <- st_within(updated_data, polygon_sf, sparse = FALSE)
      
      # Save current state before modifying for undo functionality
      segment_data$previous_data <- updated_data
      
      # Determine action based on selected radio button
      if (input$annotation_action == "mark_invalid") {
        updated_data$Outliers[points_inside] <- 1
      } else if (input$annotation_action == "mark_uncertain") {
        updated_data$Outliers[points_inside] <- 2
      } else if (input$annotation_action == "mark_valid") {
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
          "Toggling points by a polygon is only enabled in the Annotation Mode.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })
  
  # Undo the last action
observeEvent(input$undo_button, {
  if (!is.null(segment_data$previous_data)) {
    # Restore the previous state
    segment_data$data <- segment_data$previous_data
    segment_data$previous_data <- NULL # Clear the undo history

    # Refresh the map
    leafletProxy("map") %>%
      update_atl_mapleaf(segment_data$data,
                         display_non_filtered_track = FALSE,
                         zoom_flag = FALSE)
  } else {
    showModal(
      modalDialog(
        title = "Undo Not Possible",
        "No actions to undo.",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  }
})
  
  # Save the filtered data
  observeEvent(input$save_data, {
    if (input$track_display_mode == "annotation") {
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
          "Data can only be saved when the app is in Annotation Mode.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }

  })
  
  # Navigate to the next segment
  observeEvent(input$next_segment, {
    if (input$track_display_mode == "annotation") {
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
    if (input$track_display_mode == "annotation") {
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