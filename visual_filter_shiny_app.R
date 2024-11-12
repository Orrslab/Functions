# Load the Shiny and leaflet libraries
library(shiny)
library(htmltools)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)

data_for_filter <- raw_location_data

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
  if (!all(c("X", "Y", "TIME", "TAG") %in% colnames(dd))) {
    stop("Data must contain X, Y, TIME, and TAG columns")
  }
  
  # Define the color of the data points as purple
  purple_color <- "#800080"  # Hex code for purple
  
  # Convert data to sf object and transform to WGS84
  dd_sf <- st_as_sf(dd, coords = c("X", "Y"), crs = 2039) %>%
    st_transform(crs = 4326)
  
  # Get bounding box coordinates to use for zooming
  bbox <- st_bbox(dd_sf)
  
  # Create an unnamed vector for fitBounds (remove the names)
  bbox_values <- as.numeric(c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]))
  
  # Create LINESTRING for connecting points by tag
  llpd_lines <- dd_sf %>%
    group_by(TAG) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  dd_sf$dateTimeFormatted <- unix_timestamp_to_human_date(dd_sf$TIME)
  col <- brewer.pal(n = 6, name = 'Dark2')
  
  proxy %>%
    clearMarkers() %>%
    clearShapes() %>%
    addCircles(data = dd_sf, weight = 1, fillOpacity = 1, color = purple_color,
               popup = ~htmlEscape(paste0("DateTime=", dateTimeFormatted,
                                          ", Timestamp=", TIME,
                                          ", Tag Number=", sprintf("%04d", TAG %% 10000)))) %>%
    addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = purple_color) %>%
    
    # Set the map's view to fit the bounds of the data using the unnamed vector
    fitBounds(bbox_values[1], bbox_values[2], bbox_values[3], bbox_values[4])
}

# Define UI for the application
ui <- fluidPage(
  titlePanel("Visual Filter: ATLAS Data"),
  
  sidebarLayout(
    sidebarPanel(
      h2(textOutput("day_display")),
      h3("Actions"),
      actionButton("select_points", "Select Specific Points (n)"),
      actionButton("select_polygon", "Select a Polygon"),
      actionButton("filter_selection", "Filter all Selected Data (p)"),
      actionButton("next_day", "Next Day (c)"),
      actionButton("previous_day", "Previous Day (B)"),
      actionButton("discard_day", "Discard Day (b)"),
      actionButton("discard_and_select_day", "Discard and Select Day (D)") # select specific day number
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
  
  output$map <- renderLeaflet({
    initialize_atl_mapleaf()
  })
  
  observe({
    req(day_data())
    update_atl_mapleaf(leafletProxy("map"), day_data())
  })

  # output$map <- renderLeaflet({
  #   req(day_data())  # Ensure dd is available before rendering
  #   atl_mapleaf(day_data())  # Call the map function
  # })
  # 
  # # Function to update the map using atl_mapleaf
  # update_map <- function() {
  #   req(day_data())
  #   leafletProxy("map") %>%
  #     clearMarkers() %>%
  #     clearShapes()
  #   
  #   # Call atl_mapleaf with the filtered data to handle X, Y to lat/lon conversion and re-render
  #   atl_mapleaf(day_data())
  # }
  # 
  # # Use observe to trigger map update when the day changes
  # observe({
  #   update_map()  # Call the update_map function whenever day_data changes
  # })
  
  # Navigate to the next day
  observeEvent(input$next_day, {
    next_day_number <- current_day_number() + 1
    if (next_day_number <= length(day_numbers_in_data)) {
      current_day_number(next_day_number)
    }
  })
  
  observeEvent(input$previous_day, {
    previous_day_number <- current_day_number() - 1
    if (previous_day_number >= 1) {
      current_day_number(previous_day_number)
    }
  })
  
  observeEvent(input$filter_square, {
    # Code for filtering points in a square area (s)
  })
  
  observeEvent(input$filter_points, {
    # Code for removing specific points (n)
  })
  
  
  observeEvent(input$select_points, {
    # Code for selecting and filtering points in an area (p)
  })
  
  observeEvent(input$discard_day, {
    # Code for discarding the current day’s data (b)
  })
  
  observeEvent(input$go_back_day, {
    # Code for going back to the previous day (B)
  })
  
  observeEvent(input$discard_and_select_day, {
    # Code for discarding and selecting a specific day (D)
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)