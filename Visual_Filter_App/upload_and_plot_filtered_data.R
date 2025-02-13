#' Load and Visualize Filtered ATLAS Data
#'
#' This script loads movement data from an SQLite file, separates valid points from 
#' outliers and uncertain points, and visualizes them using a Leaflet map.
#'
#' The script categorizes points into three groups:
#' \itemize{
#'   \item Valid Points (Outliers == 0)
#'   \item Outliers (Outliers == 1)
#'   \item Uncertain Points (Outliers == 2)
#' }
#' These points are then plotted interactively on a map, allowing users to visually 
#' inspect their spatial distribution.
#'
#' @section User Input:
#' Before running the script, the user must specify:
#' \itemize{
#'   \item `file_name`: The name of the SQLite file containing filtered movement data.
#'   \item `file_path`: The directory where the SQLite file is stored.
#' }
#'
#' @section Dependencies:
#' The script relies on the following external scripts:
#' \itemize{
#'   \item `config_visual_filter.R` - Configuration settings for visualization.
#'   \item `load_atlas_data_from_sqlite.R` - Loads ATLAS data from an SQLite database.
#'   \item `interactive_map_single_atlas_dataset.R` - Plots a single dataset on a Leaflet map.
#'   \item `interactive_map_two_atlas_datasets.R` - Plots two datasets on a Leaflet map.
#'   \item `interactive_map_three_atlas_datasets.R` - Plots three datasets on a Leaflet map.
#' }
#'
#' @import dplyr
#' @import leaflet
#'

library(dplyr)

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# USER INPUT REQUIRED

# Set the file name and path

# Michal's data
# file_name <- "BS_0671_from_2021-10-15_03-06-19_to_2021-10-15_14-40-16_annotated.sqlite"
# file_name <- "CB_0341_from_2021-06-18_00-01-06_to_2021-06-18_02-16-02_annotated.sqlite"
# file_name <- "CD_0166_from_2021-04-30_00-01-05_to_2021-04-30_23-58-52_annotated.sqlite"
# file_name <- "CK_0161_from_2021-02-26_00-01-06_to_2021-02-26_04-35-05_annotated.sqlite"
# file_name <- "EG_0508_from_2021-05-13_06-20-33_to_2021-05-13_10-00-01_annotated.sqlite"
# file_name <- "EH_0930_from_2022-07-23_00-01-00_to_2022-07-23_02-15-24_annotated.sqlite"
# file_name <- "EJ_0366_from_2021-07-09_00-01-04_to_2021-07-09_04-39-35_annotated.sqlite"
# file_name <- "KF_0936_from_2022-12-07_04-37-06_to_2022-12-07_06-00-22_annotated.sqlite"
# file_name <- "LD_0944_from_2024-08-16_02-24-30_to_2024-08-16_05-00-21_annotated.sqlite"
# file_name <- "RW_1508_from_2024-08-15_06-00-00_to_2024-08-15_17-43-43_annotated.sqlite"
# file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Michal_Handel/Annotated_data_12-02-25/"

# Yehuda's data
# file_name <- "GJ_0722_from_2023-02-01_02-11-38_to_2023-02-01_22-57-49_annotated.sqlite"
# file_name <- "GJ_0735_from_2023-04-01_02-58-18_to_2023-04-01_23-40-00_annotated.sqlite"
# file_name <- "GJ_0739_from_2023-08-15_00-01-02_to_2023-08-15_22-45-09_annotated.sqlite"
# file_name <- "GJ_0740_from_2023-03-04_00-13-43_to_2023-03-04_23-08-36_annotated.sqlite"
# file_name <- "GJ_0745_from_2023-04-22_00-16-47_to_2023-04-22_23-59-42_annotated.sqlite"
# file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Yehuda_Samuel/Annotated_data_12-02-25/"

# Test data
# file_name <- "BO_0556_from_2021-07-10_00-00-02_to_2021-07-10_23-59-57_annotated.sqlite"
# Map debug test files
# file_name <- "BO_0556_from_2021-07-06_00-01-13_to_2021-07-06_23-59-56_annotated.sqlite"
# file_name <- "BO_0556_from_2021-07-04_17-00-03_to_2021-07-04_23-59-58_annotated.sqlite"
# file_name <- "BO_0556_from_2021-07-05_00-00-02_to_2021-07-05_23-59-49_annotated.sqlite"
file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Annotated_data_test/"

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
      map <- interactive_map_three_atlas_datasets(dd1 = outliers,
                                                  dd2 = uncertain_points,
                                                  dd3 = valid_points,
                                                  color_dd1 = color_outliers_config,
                                                  color_dd2 = color_uncertain_config,
                                                  color_dd3 = color_valid_points_config,
                                                  legendLabels = c("Outliers",
                                                                   "Uncertain Points",
                                                                   "Valid Points"))
    } else {
      # Plot the Outliers and Uncertain Points
      map <- interactive_map_two_atlas_datasets(dd1 = outliers,
                                                dd2 = uncertain_points,
                                                color_dd1 = color_outliers_config,
                                                color_dd2 = color_uncertain_config,
                                                legendLabels = c("Outliers",
                                                                 "Uncertain Points"))
    }
  } else {
    if (nrow(valid_points) > 0) {
      # Plot Valid Points and Uncertain Points
      map <- interactive_map_two_atlas_datasets(dd1 = uncertain_points,
                                                dd2 = valid_points,
                                                color_dd1 = color_uncertain_config,
                                                color_dd2 = color_valid_points_config,
                                                legendLabels = c("Uncertain Points",
                                                                 "Valid Points"))
    } else {
      # Plot only Uncertain Points
      map <- interactive_map_single_atlas_dataset(dd = uncertain_points,
                                                  color_dd = color_uncertain_config,
                                                  legendLabels = c("Uncertain Points"))
    }
  }
} else {
  if (nrow(outliers) > 0) {
    if (nrow(valid_points) > 0) {
      # Plot Valid Points and Outliers
      map <- interactive_map_two_atlas_datasets(dd1 = outliers,
                                                dd2 = valid_points,
                                                color_dd1 = color_outliers_config,
                                                color_dd2 = color_valid_points_config,
                                                legendLabels = c("Outliers",
                                                                 "Valid Points"))
    } else {
      # Plot Only Outliers
      map <- interactive_map_single_atlas_dataset(dd = outliers,
                                                  color_dd = color_outliers_config,
                                                  legendLabels = c("Outliers"))
    }
  } else {
    if (nrow(valid_points) > 0) {
      # Plot only Valid Points
      map <- interactive_map_single_atlas_dataset(dd = valid_points,
                                                  color_dd = color_valid_points_config,
                                                  legendLabels = c("Valid Points"))
    } else {
      stop("No data to plot.")
    }
  }
}
  
print(map)
  