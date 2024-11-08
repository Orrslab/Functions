# Load the Shiny and leaflet libraries
library(shiny)
library(htmltools)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Visual Movement Data Filter"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Actions"),
      actionButton("next_day", "Next Day (c)"),
      actionButton("zoom_in", "Zoom In (i)"),
      actionButton("zoom_out", "Zoom Out (o)"),
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
  # Placeholder for data, filtered points, and collected points
  dd1 <- reactive({raw_location_data})
  dd2 <- reactive({raw_location_data})
  
  # Leaflet map output
  source(paste0(path_to_atlas_data_analysis_repo,"ATLAS_maps/", "interactive_maps.R"))
  source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))
  
  output$map <- renderLeaflet({
    req(dd1(), dd2())  # Ensure dd1 and dd2 are available before rendering
    
    atl_mapleaf2(dd1(), dd2())  # Call your map function
  })
  
  # Observe button click to trigger map updates or any action
  observeEvent(input$update_map, {
    # You can update your map or data here
    # If dd1 or dd2 need to change dynamically, you can modify them here
    print("Map Updated!")  # Example action
    output$map <- renderLeaflet({
      atl_mapleaf2(dd1(), dd2())  # Re-render the map
    })
  })
  
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

# Run the application 
shinyApp(ui = ui, server = server)