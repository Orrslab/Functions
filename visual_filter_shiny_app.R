# Load the Shiny and leaflet libraries
library(shiny)
library(htmltools)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)

# TODO: Upload the data from a csv file
data_for_filter <- raw_location_data

# Add a 'Outliers' column with the values 0 for good points, and 1 for outliers
data_for_filter$Outliers <- 0

# Apply the baseline filter on the Speed, STD, and number of base stations
source(paste0(path_to_atlas_data_analysis_repo, "apply_speed_std_nbs_filter.R"))
data_for_filter <- apply_speed_std_nbs_filter(data_for_filter)

# Scripts for the leaflet map
source(paste0(path_to_atlas_data_analysis_repo,"ATLAS_maps/", "interactive_maps.R"))
source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))

# Helper function to initialize the base map
initialize_atl_mapleaf <- function(MapProvider='Esri.WorldImagery') {
  leaflet() %>%
    addProviderTiles(MapProvider) %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE, maxWidth = 200))
}

# Helper function to update the map with data
update_atl_mapleaf <- function(proxy, dd) {

  # Ensure that the required columns are present in the dataset
  if (!all(c("X", "Y", "TIME", "TAG", "Outliers") %in% colnames(dd))) {
    stop("Data must contain X, Y, TIME, TAG, and Outliers columns")
  }
  
  # Define the colors for valid points (purple) and outliers (yellow)
  color_valid_points <- "#800080"  # Purple color
  color_outliers <- "yellow"
  
  # Filter out the outliers (non-outliers will be used to create lines)
  dd_non_outliers <- dd %>% filter(Outliers == 0)
  dd_outliers <- dd %>% filter(Outliers == 1)
  
  # Convert data to sf object and transform to WGS84 (EPSG:4326)
  dd_sf <- st_as_sf(dd, coords = c("X", "Y"), crs = 2039) %>%
    st_transform(crs = 4326)
  
  dd_non_outliers_sf <- st_as_sf(dd_non_outliers, coords = c("X", "Y"), crs = 2039) %>%
    st_transform(crs = 4326)
  
  dd_outliers_sf <- st_as_sf(dd_outliers, coords = c("X", "Y"), crs = 2039) %>%
    st_transform(crs = 4326)
  
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
    addCircleMarkers(data = dd_outliers_sf, weight = 1, fillOpacity = 1, color = color_outliers, radius=2,
                     # label = ~htmlEscape(paste0("DateTime=", dateTimeFormatted,
                     #                            ", Timestamp=", TIME,
                     #                            ", Tag Number=", sprintf("%04d", TAG %% 10000))),
                     # labelOptions = labelOptions(
                     #   direction = "auto",
                     #   opacity = 0.9,
                     #   offset = c(10, 10),
                     #   style = list(
                     #     "background-color" = "white",
                     #     "border" = "1px solid black",
                     #     "padding" = "3px",
                     #     "border-radius" = "3px"
                     #   )
                     # )
                     ) %>%
    
    # Add non-outliers with purple color
    addCircleMarkers(data = dd_non_outliers_sf, weight = 1, fillOpacity = 1, color = color_valid_points, radius=2,
                     # label = ~htmlEscape(paste0("DateTime=", dateTimeFormatted,
                     #                            ", Timestamp=", TIME,
                     #                            ", Tag Number=", sprintf("%04d", TAG %% 10000))),
                     # labelOptions = labelOptions(
                     #   direction = "auto",
                     #   opacity = 0.9,
                     #   offset = c(10, 10),
                     #   style = list(
                     #     "background-color" = "white",
                     #     "border" = "1px solid black",
                     #     "padding" = "3px",
                     #     "border-radius" = "3px"
                     #   )
                     # )
                     ) %>%
    
    # Add lines connecting non-outliers
    addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = color_valid_points) %>%
    
    # Set the map's view to fit the bounds of the data
    fitBounds(bbox_values[1], bbox_values[2], bbox_values[3], bbox_values[4])
}

# Define UI for the application
ui <- fluidPage(
  titlePanel("Visual Filter: ATLAS Data"),
  
  sidebarLayout(
    sidebarPanel(
      h2(textOutput("day_display")),
      h3("Actions"),
      actionButton("select_polygon", "Select a Polygon"),
      actionButton("filter_selection", "Filter all Selected Data"),
      actionButton("next_day", "Next Day"),
      actionButton("previous_day", "Previous Day")
    ),
    
    mainPanel(
      leafletOutput("map", width = "100%", height = 600),
      tableOutput("filtered_data")
    )
  )
)

# Define server logic for the application
server <- function(input, output, session) {
  # Get the list of the days and tags, and validate that the input is sufficient to run the app
  day_numbers_in_data <- unique(data_for_filter$DAY)
  if(is.null(day_numbers_in_data)) 
  {Er <- simpleError("No DAY numbers were found in the provided data.")
  stop(Er)}
  
  tag_numbers_in_data <- as.numeric(unique(data_for_filter$TAG))
  if(is.null(tag_numbers_in_data)) 
  {Er <- simpleError("No TAG numbers were found in the provided data.")
  stop(Er)}
  
  # Return an error in case the data includes more than a single tag number
  if(length(tag_numbers_in_data)>1) 
  {Er <- simpleWarning("Please use data from a single tag number. This app handles data from one tag at a time.")
  stop(Er)}
  
  # define a reactive variable for the day number which will be displayed above the map
  current_day_number <- reactiveVal(day_numbers_in_data[1])
  
  # Display the current day number in the UI
  output$day_display <- renderText({
    paste("Day", current_day_number())
  })
  
  # Reactive data frame for the data of the current day
  day_data <- reactive(data_for_filter[data_for_filter$DAY==current_day_number(), ])
  
  # A list to store dynamic data frames for each day
  selected_points <- reactiveVal(list())
  
  output$map <- renderLeaflet({
    initialize_atl_mapleaf()
  })
  
  observe({
    req(day_data())
    update_atl_mapleaf(leafletProxy("map"), day_data())
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