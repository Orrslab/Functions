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

# Upload the data from a csv file
file_name <- "BO_0430_from_2021-08-23_17-00-00_to_2021-08-24_05-00-00_raw.csv"
file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Raw_tracks/"
full_path <- paste0(file_path, file_name)
data_for_filter <- read.csv(full_path)

# Add a 'Outliers' column with the values 0 for good points, and 1 for outliers
data_for_filter$Outliers <- 0

# Apply the baseline filter on the Speed, STD, and number of base stations
source(paste0(path_to_atlas_data_analysis_repo, "apply_speed_std_nbs_filter.R"))
data_for_filter <- apply_speed_std_nbs_filter(data_for_filter)

# # add latitude and longitude columns to the data

# Create an sf object with the projected CRS (EPSG:2039)
data_for_filter_sf <- st_as_sf(data_for_filter, coords = c("X", "Y"), crs = 2039)

# Transform the coordinates to EPSG:4326 (WGS84)
data_for_filter_sf <- st_transform(data_for_filter_sf, crs = 4326)

# Extract the transformed lat and lon columns
data_for_filter_sf$lat <- st_coordinates(data_for_filter_sf)[, 2]
data_for_filter_sf$lon <- st_coordinates(data_for_filter_sf)[, 1]

# Scripts for the leaflet map
source(paste0(path_to_atlas_data_analysis_repo,"ATLAS_maps/", "interactive_maps.R"))
source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))

# Helper function to initialize the base map
initialize_atl_mapleaf <- function(MapProvider='Esri.WorldImagery', tile_opacity = 0.8) {
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
      editOptions = NULL # Remove edit and delete buttons
    ) %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE, maxWidth = 200))
  }

# Helper function to update the map with data
update_atl_mapleaf <- function(proxy, dd_sf, zoom_flag = TRUE) {

  # Ensure that the required columns are present in the dataset
  if (!all(c("lon", "lat", "TIME", "TAG", "Outliers") %in% colnames(dd_sf))) {
    stop("Data must contain lon, lat, TIME, TAG, and Outliers columns")
  }
  
  # Define the colors for valid points (purple) and outliers (yellow)
  color_valid_points <- "#800080"  # Purple color
  color_outliers <- "yellow"
  
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
  
  # Create LINESTRING for connecting non-outliers points by tag
  llpd_lines <- dd_non_outliers_sf %>%
    group_by(TAG) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  proxy %>%
    clearMarkers() %>%
    clearShapes() %>%
    
    # Add outliers with yellow color
    addCircleMarkers(data = dd_outliers_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_outliers, radius=4,
                     label = ~htmlEscape(paste0(dateTimeFormatted)),
                     # label = ~htmlEscape(paste0("DateTime=", dateTimeFormatted,
                     #                            ", Timestamp=", TIME,
                     #                            ", Tag Number=", sprintf("%04d", TAG %% 10000))),
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
    
    # Add non-outliers with purple color
    addCircleMarkers(data = dd_non_outliers_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_valid_points, radius=4,
                     label = ~htmlEscape(paste0(dateTimeFormatted)),
                     # label = ~htmlEscape(paste0("DateTime=", dateTimeFormatted,
                     #                            # ", Timestamp=", TIME,
                     #                            ", Tag Number=", sprintf("%04d", TAG %% 10000))),
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
      title = "Point Types",
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

# Define UI for the application
ui <- fluidPage(
  titlePanel("Visual Filter: ATLAS Data"),
  
  sidebarLayout(
    sidebarPanel(
      h2(textOutput("day_display")),
      h3("Actions"),
      actionButton("filter_selection", "Filter all Selected Data"),
      actionButton("next_day", "Next Day"),
      actionButton("previous_day", "Previous Day")
    ),
    
    mainPanel(
      leafletOutput("map", width = "100%", height = 600),
      # tableOutput("filtered_data")
    )
  )
)

# Define server logic for the application
server <- function(input, output, session) {
  # Get the list of the days and tags, and validate that the input is sufficient to run the app
  day_numbers_in_data <- unique(data_for_filter_sf$DAY)
  if(is.null(day_numbers_in_data)) 
  {Er <- simpleError("No DAY numbers were found in the provided data.")
  stop(Er)}
  
  tag_numbers_in_data <- as.numeric(unique(data_for_filter_sf$TAG))
  if(is.null(tag_numbers_in_data)) 
  {Er <- simpleError("No TAG numbers were found in the provided data.")
  stop(Er)}
  
  # Return an error in case the data includes more than a single tag number
  if(length(tag_numbers_in_data)>1) 
  {Er <- simpleWarning("Please use data from a single tag number. This app handles data from one tag at a time.")
  stop(Er)}
  
  # define a reactive variable for the day number which will be displayed above the map
  current_day_number <- reactiveVal(day_numbers_in_data[1])
  
  # Reactive data frame for the data of the current day
  day_data <- reactiveValues(data = data_for_filter_sf[data_for_filter_sf$DAY==day_numbers_in_data[1], ])

  # Display the current day number in the UI
  output$day_display <- renderText({
    paste("Day", current_day_number())
  })
  
  # Render the initial map
  output$map <- renderLeaflet({
    initialize_atl_mapleaf()
  })
  
  # Update the map when the current day changes
  observeEvent(current_day_number(), {
    # Update day_data for the current day
    day_data$data <- data_for_filter_sf[data_for_filter_sf$DAY == current_day_number(), ]
    
    # Update the map with new day's data
    leafletProxy("map") %>%
      update_atl_mapleaf(day_data$data)
  })
  
  # Toggle a point when clicked
  observeEvent(input$map_marker_click, {
    
    clicked_timestamp <- input$map_marker_click$id # This is now the timestamp of the clicked marker
    
    # Find the index of the clicked point
    current_data <- day_data$data
    index <- which(current_data$TIME == as.numeric(clicked_timestamp))
    
    if (length(index) == 1) { # Ensure only one point matches
      # Toggle Outliers for the clicked point
      current_data$Outliers[index] <- ifelse(current_data$Outliers[index] == 0, 1, 0)
      
      day_data$data <- current_data
    
      leafletProxy("map") %>%
        update_atl_mapleaf(day_data$data, zoom_flag = FALSE)
      
    }
  
  })
  
  # Select outliers by polygon
  observeEvent(input$map_draw_new_feature, {
    # Get the drawn polygon's coordinates
    polygon_coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
    polygon_sf <- st_polygon(list(matrix(unlist(polygon_coords), ncol = 2, byrow = TRUE)))
    polygon_sf <- st_sfc(polygon_sf, crs = 4326)
    
    # Filter points inside the polygon, and mark them as Outliers = 1
    updated_data <- day_data$data
    points_inside <- st_within(updated_data, polygon_sf, sparse = FALSE)
    updated_data$Outliers[points_inside] <- 1
    # Update the reactive data variable
    day_data$data <- updated_data
    
    # Refresh the map
    leafletProxy("map") %>%
      update_atl_mapleaf(day_data$data, zoom_flag = FALSE)
  })
  
  # Navigate to the next day
  observeEvent(input$next_day, {
    next_day_number <- current_day_number() + 1
    if (next_day_number <= length(day_numbers_in_data)) {
      current_day_number(next_day_number)
    }
  })
  
  # Navigate to the previous day
  observeEvent(input$previous_day, {
    previous_day_number <- current_day_number() - 1
    if (previous_day_number >= 1) {
      current_day_number(previous_day_number)
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)