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

# USER INPUT REQUIRED
# Set the file name and path
file_name <- "BO_0556_from_2021-07-04_17-00-03_to_2021-07-04_23-59-58_filtered.sqlite"
file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Filtered_data/"
# END OF USER INPUT

# Generate the full path
full_path <- paste0(file_path, file_name)

# Load the filtered data from sqlite
source(paste0(getwd(), "/load_atlas_data_from_sqlite.R"))
filtered_data <- load_atlas_data_from_sqlite(full_path)

# Remove rows with any NA values
filtered_data_clean <- na.omit(filtered_data)

# Extract the valid points and outliers
valid_points <- filtered_data_clean[filtered_data$Outliers == 0, ]
outliers <- filtered_data_clean[filtered_data$Outliers == 1, ]

# Plot the valid points versus outliers on a leaflet map
source(paste0(getwd(), "/ATLAS_maps/", "interactive_map_two_atlas_datasets.R"))
map <- interactive_map_two_atlas_datasets(dd1 = valid_points,
                                          dd2 = outliers,
                                          MapProvider='CartoDB.Positron',  # 'Esri.WorldImagery'
                                          legendLabels=c("Valid Points", "Outliers")) 
print(map)
  