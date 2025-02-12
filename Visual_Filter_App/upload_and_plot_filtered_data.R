#' Load, Clean, and Visualize Filtered ATLAS Data
#'
#' This script loads movement data from an SQLite file, removes missing values, 
#' separates valid points from outliers, and visualizes them using a Leaflet map.
#'
#' @note Ensure that `load_atlas_data_from_sqlite.R` and `interactive_map_two_atlas_datasets.R` 
#' are in the working directory before running this script.
#'
#' @import leaflet
#'
#' @example 
#' \dontrun{
#' # Set file name and path
#' file_name <- "BO_0556_from_2021-07-04_17-00-03_to_2021-07-04_23-59-58_filtered.sqlite"
#' file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Filtered_data/"
#'
#' # Run the script to generate an interactive map
#' source("this_script.R")
#' }

library(dplyr)

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# USER INPUT REQUIRED
# Set the file name and path
file_name <- "WB_0547_from_2021-06-05_04-54-33_to_2021-06-05_18-35-50_annotated.sqlite"
file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Michal_Handel/Annotated_data_12-02-25/"

# Define the path for sourcing files from the Visual Filter App's folder
# path_for_sourcing <- getwd()
path_for_sourcing <- paste0(getwd(), "/Visual_Filter_App/")

# END OF USER INPUT

source(paste0(path_for_sourcing, "config_visual_filter.R"))

# Generate the full path
full_path <- paste0(file_path, file_name)

# Load the filtered data from sqlite
source(paste0(getwd(), "/load_atlas_data_from_sqlite.R"))
filtered_data <- load_atlas_data_from_sqlite(full_path)

# Extract the valid points, outliers and uncertain points
valid_points <- filtered_data %>% filter(Outliers == 0)
outliers <- filtered_data %>% filter(Outliers == 1)
uncertain_points <- filtered_data %>% filter(Outliers == 2)

# Source the plotting functions
source(paste0(path_for_sourcing, "/interactive_map_single_atlas_dataset.R"))
source(paste0(path_for_sourcing, "/interactive_map_two_atlas_datasets.R"))
source(paste0(path_for_sourcing, "/interactive_map_three_atlas_datasets.R"))

# Plot the valid points versus outliers on a leaflet map
if (nrow(uncertain_points) > 0) {
  if (nrow(outliers) > 0) {
    if (nrow(valid_points) > 0) {
      # Plot the Valid Points, Outliers and Uncertain Points
      map <- interactive_map_three_atlas_datasets(dd1 = valid_points,
                                                  dd2 = outliers,
                                                  dd3 = uncertain_points,
                                                  legendLabels = c("Valid Points",
                                                                   "Outliers",
                                                                   "Uncertain Points"))
    } else {
      # Plot the Outliers and Uncertain Points
      map <- interactive_map_two_atlas_datasets(dd1 = outliers,
                                                dd2 = uncertain_points,
                                                legendLabels = c("Outliers",
                                                                 "Uncertain Points"))
    }
  } else {
    if (nrow(valid_points) > 0) {
      # Plot Valid Points and Uncertain Points
      map <- interactive_map_two_atlas_datasets(dd1 = valid_points,
                                                dd2 = uncertain_points,
                                                legendLabels = c("Valid Points",
                                                                 "Uncertain Points"))
    } else {
      # Plot only Uncertain Points
      map <- interactive_map_single_atlas_dataset(dd = uncertain_points,
                                                  legendLabels = c("Uncertain Points"))
    }
  }
} else {
  if (nrow(outliers) > 0) {
    if (nrow(valid_points) > 0) {
      # Plot Valid Points and Outliers
      map <- interactive_map_two_atlas_datasets(dd1 = valid_points,
                                                dd2 = outliers,
                                                legendLabels = c("Valid Points",
                                                                 "Outliers"))
    } else {
      # Plot Only Outliers
      map <- interactive_map_single_atlas_dataset(dd = outliers,
                                                  legendLabels = c("Outliers"))
    }
  } else {
    if (nrow(valid_points) > 0) {
      # Plot only Valid Points
      map <- interactive_map_single_atlas_dataset(dd = valid_points,
                                                  legendLabels = c("Valid Points"))
    } else {
      stop("No data to plot.")
    }
  }
}
  
print(map)
  