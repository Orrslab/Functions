# Helper function to update the map with data
update_atl_mapleaf <- function(proxy, dd_sf, 
                               display_non_filtered_track, 
                               zoom_flag = TRUE, 
                               color_outliers = color_outliers_config,
                               color_uncertain = color_uncertain_config) {
  
  # Ensure that the required columns are present in the dataset
  if (!all(c("lon", "lat", "TIME", "TAG", "Outliers") %in% colnames(dd_sf))) {
    stop("Data must contain lon, lat, TIME, TAG, and Outliers columns")
  }
  
  # Define the color for valid points
  color_valid_points <- color_valid_points_config
  
  # Filter out the Valid Points, Outliers and Uncertain Points
  dd_non_outliers_sf <- dd_sf %>% filter(Outliers == 0)
  dd_outliers_sf <- dd_sf %>% filter(Outliers == 1)
  dd_uncertain_sf <- dd_sf %>% filter(Outliers == 2)
  
  # Create dateTimeFormatted from TIME column if not already present
  if (!"dateTimeFormatted" %in% colnames(dd_sf)) {
    dd_sf <- dd_sf %>%
      mutate(dateTimeFormatted = unix_timestamp_to_human_date(TIME))  # Ensure conversion happens
  }
  
  if (!"dateTimeFormatted" %in% colnames(dd_uncertain_sf)) {
    dd_uncertain_sf <- dd_uncertain_sf %>%
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
    
    # Normalize the TIME column of the complete dataset
    time_min <- min(dd_sf$TIME, na.rm = TRUE)
    time_max <- max(dd_sf$TIME, na.rm = TRUE)
    
    # Avoid division by zero if all times are the same
    if (time_min == time_max) {
      time_normalized <- rep(0.5, nrow(dd_sf))  # All points have the same time
    } else {
      time_normalized <- (dd_sf$TIME - time_min) / (time_max - time_min)
    }
    
    # Define a custom color gradient from color1 to color2- color-blind friendly
    color1 <- color_track_start_config
    color2 <- color_track_end_config
    line_color <- color_connecting_line_config
    
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
    
    #####
    
    ## Get the start and end points of the entire time series to mark them on the map
    
    # Ensure the timestamp column is numeric
    dd_sf_timestamp <- as.numeric(dd_sf$TIME)
    
    # Find min and max timestamps
    min_time <- min(dd_sf_timestamp, na.rm = TRUE)
    max_time <- max(dd_sf_timestamp, na.rm = TRUE)
    
    # Extract rows with min and max timestamps
    first_point <- dd_sf[dd_sf_timestamp == min_time, ]
    last_point <- dd_sf[dd_sf_timestamp == max_time, ]
    
    # Combine them into one dataframe
    first_last_points <- rbind(first_point, last_point)
    
    #####
    
    # # Create LINESTRING for connecting the valid points by tag
    llpd_lines <- dd_non_outliers_sf %>%
      group_by(TAG) %>%
      summarize(do_union = FALSE) %>%
      st_cast("LINESTRING")
    
    # If there are no outliers in the data set:
    if (!nrow(dd_outliers_sf) > 0) {
      
      # If there are no Uncertain points in the data set
      if (!nrow(dd_uncertain_sf) > 0) {
        
        # Plot only the valid points
        proxy %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%  # Clear existing controls (including the legend)
          
          # Add the start and end points
          addCircleMarkers(data = first_last_points, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_start_end_points, radius = srart_end_points_radius,
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
          
          # Add lines connecting the valid points
          addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = color_connecting_line_valid_points_config) %>%
          
          # Add the valid points
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
          
          # Add a legend to the map
          addLegend(
            position = "topright",
            colors = c(color_valid_points, color_uncertain, color_outliers, color_start_end_points),
            labels = c("Valid Points", "Uncertain Points", "Outliers", "Start or End Point"),
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
        
        # Plot the Valid and Uncertain points
        proxy %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%  # Clear existing controls (including the legend)
          
          # Add the start and end points
          addCircleMarkers(data = first_last_points, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_start_end_points, radius = srart_end_points_radius,
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
          
          # Add Uncertain points
          addCircleMarkers(data = dd_uncertain_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_uncertain, radius=4,
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
          addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = color_connecting_line_valid_points_config) %>%
          
          # Add valid points
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
          
          # Add a legend to the map
          addLegend(
            position = "topright",
            colors = c(color_valid_points, color_uncertain, color_outliers, color_start_end_points),
            labels = c("Valid Points", "Uncertain Points", "Outliers", "Start or End Point"),
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
      
    } else if (!nrow(dd_non_outliers_sf) > 0) {
      # If there are no valid points in the data set:
      
      # If there are no Uncertain points in the data set:
      if (!nrow(dd_uncertain_sf) > 0) {
        
        # Plot only the Outliers
        proxy %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%  # Clear existing controls (including the legend)
          
          # Add the start and end points
          addCircleMarkers(data = first_last_points, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_start_end_points, radius = srart_end_points_radius,
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
          
          # Add outliers markers
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
            colors = c(color_valid_points, color_uncertain, color_outliers, color_start_end_points),
            labels = c("Valid Points", "Uncertain_Points", "Outliers", "Start or End Point"),
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
        
        # Plot the Uncertain Points and Outliers
        proxy %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%  # Clear existing controls (including the legend)
          
          # Add the start and end points
          addCircleMarkers(data = first_last_points, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_start_end_points, radius = srart_end_points_radius,
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
          
          # Add Uncertain points
          addCircleMarkers(data = dd_uncertain_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_uncertain, radius=4,
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
          
          # Add outliers
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
            colors = c(color_valid_points, color_uncertain, color_outliers, color_start_end_points),
            labels = c("Valid Points", "Uncertain_Points", "Outliers", "Start or End Point"),
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
      
    } else {
      
      # If there are no Uncertain Points in the data set:
      if (!nrow(dd_non_outliers_sf) > 0) {
        
        # Plot the outliers and valid points to the map
        proxy %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%  # Clear existing controls (including the legend)
          
          # Add the start and end points
          addCircleMarkers(data = first_last_points, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_start_end_points, radius = srart_end_points_radius,
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
          
          # Add outliers markers
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
          
          # Add lines connecting the valid points
          addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = color_connecting_line_valid_points_config) %>%
          
          # Add the valid points markers
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
          
          # Add a legend to the map
          addLegend(
            position = "topright",
            colors = c(color_valid_points, color_uncertain, color_outliers, color_start_end_points),
            labels = c("Valid Points", "Uncertain Points", "Outliers", "Start or End Point"),
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
        
        # Plot the Valid Points, Uncertain Points and Outliers
        proxy %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%  # Clear existing controls (including the legend)
          
          # Add the start and end points
          addCircleMarkers(data = first_last_points, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_start_end_points, radius = srart_end_points_radius,
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
          
          # Add Uncertain points
          addCircleMarkers(data = dd_uncertain_sf, weight = 1, fillOpacity = 1, layerId = ~TIME, color = color_uncertain, radius=4,
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
          
          # Add outliers markers
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
          
          # Add lines connecting non-outliers
          addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = color_connecting_line_valid_points_config) %>%
          
          # Add the valid points markers
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
          
          # Add a legend to the map
          addLegend(
            position = "topright",
            colors = c(color_valid_points, color_uncertain, color_outliers, color_start_end_points),
            labels = c("Valid Points", "Uncertain Points", "Outliers", "Start or End Point"),
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
}