

library(dplyr)
library(leaflet)

plot_visual_filter_data <- function(data, 
                                    color_valid_points = "#5D3A9B",
                                    color_outliers = "#E66100",
                                    color_uncertain_points = "#FFB000") {
  
  # Check if Outliers column exists
  if (!"Outliers" %in% colnames(data)) {
    stop("The dataframe must have an 'Outliers' column.")
  }
  
  # Extract the valid points, outliers and uncertain points
  valid_points <- data %>% filter(Outliers == 0)
  outliers <- data %>% filter(Outliers == 1)
  uncertain_points <- data %>% filter(Outliers == 2)
  
  # Source the plotting functions
  source(paste0(getwd(), "/Mapping_tools/interactive_map_single_atlas_dataset.R"))
  source(paste0(getwd(), "/Mapping_tools/interactive_map_two_atlas_datasets.R"))
  source(paste0(getwd(), "/Mapping_tools/interactive_map_three_atlas_datasets.R"))
  
  # Plot the valid points versus outliers on a leaflet map
  if (nrow(uncertain_points) > 0) {
    if (nrow(outliers) > 0) {
      if (nrow(valid_points) > 0) {
        # Plot the Valid Points, Outliers and Uncertain Points
        map <- interactive_map_three_atlas_datasets(dd1 = outliers,
                                                    dd2 = uncertain_points,
                                                    dd3 = valid_points,
                                                    color_dd1 = color_outliers,
                                                    color_dd2 = color_uncertain_points,
                                                    color_dd3 = color_valid_points,
                                                    legendLabels = c("Outliers",
                                                                     "Uncertain Points",
                                                                     "Valid Points"))
      } else {
        # Plot the Outliers and Uncertain Points
        map <- interactive_map_two_atlas_datasets(dd1 = outliers,
                                                  dd2 = uncertain_points,
                                                  color_dd1 = color_outliers,
                                                  color_dd2 = color_uncertain_points,
                                                  legendLabels = c("Outliers",
                                                                   "Uncertain Points"))
      }
    } else {
      if (nrow(valid_points) > 0) {
        # Plot Valid Points and Uncertain Points
        map <- interactive_map_two_atlas_datasets(dd1 = uncertain_points,
                                                  dd2 = valid_points,
                                                  color_dd1 = color_uncertain_points,
                                                  color_dd2 = color_valid_points,
                                                  legendLabels = c("Uncertain Points",
                                                                   "Valid Points"))
      } else {
        # Plot only Uncertain Points
        map <- interactive_map_single_atlas_dataset(dd = uncertain_points,
                                                    color_dd = color_uncertain_points,
                                                    legendLabels = c("Uncertain Points"))
      }
    }
  } else {
    if (nrow(outliers) > 0) {
      if (nrow(valid_points) > 0) {
        # Plot Valid Points and Outliers
        map <- interactive_map_two_atlas_datasets(dd1 = outliers,
                                                  dd2 = valid_points,
                                                  color_dd1 = color_outliers,
                                                  color_dd2 = color_valid_points,
                                                  legendLabels = c("Outliers",
                                                                   "Valid Points"))
      } else {
        # Plot Only Outliers
        map <- interactive_map_single_atlas_dataset(dd = outliers,
                                                    color_dd = color_outliers,
                                                    legendLabels = c("Outliers"))
      }
    } else {
      if (nrow(valid_points) > 0) {
        # Plot only Valid Points
        map <- interactive_map_single_atlas_dataset(dd = valid_points,
                                                    color_dd = color_valid_points,
                                                    legendLabels = c("Valid Points"))
      } else {
        stop("No data to plot.")
      }
    }
  }
  
  print(map)
  
  return(map)
}

