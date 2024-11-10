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

# Define UI for the application
ui <- fluidPage(
  titlePanel("Visual Filter: ATLAS Data"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Actions"),
      actionButton("next_day", "Next Day (c)"),
      actionButton("filter_square", "Filter Square Area (s)"),
      actionButton("filter_points", "Remove Specific Points (n)"),
      actionButton("draw_line", "Draw Line and Filter (l)"),
      actionButton("select_points", "Select and Filter Points (p)"),
      actionButton("discard_day", "Discard Day (b)"),
      actionButton("go_back_day", "Go Back to Previous Day (B)"),
      actionButton("discard_and_select_day", "Discard and Select Day (D)"),
      actionButton("set_default_filter", "Set Default Filter Points (t)"),
      actionButton("toggle_display", "Toggle Display Options (d)")
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
  
  # Filter the data from each day separately
  for (day_num in day_numbers_in_data) {

    # Placeholder for data, filtered points, and collected points
    day_data <- reactive(data_for_filter[data_for_filter$DAY==day_num, ])
    
    output$map <- renderLeaflet({
      req(day_data())  # Ensure dd is available before rendering
      
      atl_mapleaf(day_data())  # Call the map function
    })
    
    # # Observe button click to trigger map updates or any action
    # observeEvent(input$update_map, {
    #   # You can update your map or data here
    #   # If dd1 or dd2 need to change dynamically, you can modify them here
    #   print("Map Updated!")  # Example action
    #   output$map <- renderLeaflet({
    #     atl_mapleaf2(dd1(), dd2())  # Re-render the map
    #   })
    # })
    
    # Observers for each action
    observeEvent(input$next_day, {
      # Code for showing the next day’s movement data (c)
    })
    
    observeEvent(input$zoom_in, {
      # Code for zooming in to a specified area (i)
    })
    
    observeEvent(input$zoom_out, {
      # Code for zooming out to the original view (o)
    })
    
    observeEvent(input$filter_square, {
      # Code for filtering points in a square area (s)
    })
    
    observeEvent(input$filter_points, {
      # Code for removing specific points (n)
    })
    
    observeEvent(input$draw_line, {
      # Code for drawing a line and filtering points (l)
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
    
    observeEvent(input$set_default_filter, {
      # Code for setting the default number of points to filter out (t)
    })
    
    observeEvent(input$toggle_display, {
      # Code for toggling display options (d)
    })
    
  }

}

# Run the application 
shinyApp(ui = ui, server = server)