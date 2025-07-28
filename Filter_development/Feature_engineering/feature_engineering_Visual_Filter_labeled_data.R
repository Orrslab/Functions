# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(readxl)
library(dplyr)
library(data.table)

source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
source(file.path(getwd(), "check_and_clean_duplicates_in_localizations.R"))
source(file.path(getwd(), "atlas_metrics.R"))
source(file.path(getwd(), "Filter_development", "Feature_engineering", "calculate_point_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_time_window_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_post_window_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/save_ATLAS_data_with_features_to_sqlite.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Labeled_data_DB/Visual_Filter_DB"
path_to_species_metadata <- file.path(path_to_db, "Species_metadata.xlsx")
folder_of_beacons_info_tables <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Filter_development/Feature_engineering"
filename_beacons_detection_ratio_table <- "beacons_detection_ratio_per_hour.Rds"
filename_base_stations_summary_per_beacon <- "base_stations_summary_per_beacon.Rds"
folder_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"

# For the Stops Filter
# path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Stops_Filter/Stops_DB"
# path_to_species_metadata <- file.path(path_to_db, "Species_metadata.xlsx")
# folder_of_beacons_info_tables <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Filter_development/Feature_engineering"
# filename_beacons_detection_ratio_table <- "beacons_detection_ratio_per_hour.Rds"
# filename_base_stations_summary_per_beacon <- "base_stations_summary_per_beacon.Rds"
# folder_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Stops_Filter/Stops_analysis/Monivg_vs_stopping_barn_owls"

# # Define the time gap between tracks in seconds. 
# # This is the time gap that most likely distinguished between different trakectories of the same animal.
# gap_between_tracks_sec <- 600 
# 
half_time_window_size_sec <- 25

low_beacon_detection_fraction <- 0.6

### USER INPUT END

# Load species metadata
species_metadata <- read_excel(path_to_species_metadata)

# Load the hourly detections counts of the beacons
base_stations_summary_per_beacon <- readRDS(file.path(folder_of_beacons_info_tables, filename_base_stations_summary_per_beacon))

# Load the beacons detection ratio table
beacons_detection_ratio_per_hour <- readRDS(file.path(folder_of_beacons_info_tables, filename_beacons_detection_ratio_table))

# all_data <- load_atlas_data_from_sqlite(file.path(path_to_db, "labeled_data_db.sqlite"))

# Run on the species
for (species_id in species_metadata$Species_ID) {
  
  # For debug purposes
  # species_id <- "LD"
  # species_id <- "CB"
  # species_id <- "RW"
  # species_id <- "GJ"
  
  print(species_id)
  
  # Set the species' sqlite file name and path
  file_name <- paste0(species_id, "_labeled_data.sqlite")
  file_path <- file.path(path_to_db, "Combined_species_data", file_name)
  
  # Load the species data from the sqlite file
  data <- load_atlas_data_from_sqlite(file_path)
  
  # Extract the localizations and detections data
  localization_data <- data$LOCALIZATIONS
  detection_data <- data$DETECTIONS
  
  ### DEBUGGING
  # localization_data <- localization_data[1:50000, ]
  ###
  
  # Delete duplicates from the localizations data- in case there are duplicates
  localization_data <- check_and_clean_duplicates_in_localizations(
    localization_data = localization_data,
    clean_duplicates = FALSE)
  
  # Convert dateTime to a human-readable format
  localization_data$dateTime <- as.POSIXct(localization_data$dateTime, origin = "1970-01-01", tz = "UTC")
  
  # Calculate the time difference between consecutive points
  localization_data$time_diff_sec <- calculate_time_diff(localization_data$TIME)
  
  # Calculate the point-based_features
  results <- calculate_point_based_features(localization_data, 
                                            detection_data, 
                                            beacons_detection_ratio_per_hour,
                                            base_stations_summary_per_beacon,
                                            low_beacon_detection_fraction)
  
  # Extract the results
  localization_data <- results$localization_data
  participating_base_stations <- results$participating_base_stations
  missed_base_stations <- results$missed_base_stations

  # Calculate the time-window-based features
  localization_data <- calculate_time_window_based_features(localization_data = localization_data,
                                                            half_window_size_sec = half_time_window_size_sec)

  # Calculate features that require values of other point-based and window-based features
  localization_data <- calculate_post_window_features(localization_data)
  
  # Add the species_id to the data tables
  localization_data$Species_id <- species_id
  detection_data$Species_id <- species_id
  participating_base_stations$Species_id <- species_id
  missed_base_stations$Species_id <- species_id
  
  # Print the first rows of localization_data
  # print("Final LOCALIZATIONS with features:")
  # print(colnames(localization_data))
  # print(head(localization_data, 20))
  
  print(colnames(localization_data))
  
  ## Save the data as sqlite
  output_file_name <- paste0(species_id, "_features_eng.sqlite")
  save_ATLAS_data_with_features_to_sqlite(
    localization_data = localization_data,
    detections_data = detection_data,
    participating_base_stations = participating_base_stations,
    missed_base_stations = missed_base_stations,
    fullpath = file.path(folder_to_save_results, output_file_name))
}
