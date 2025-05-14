# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(readxl)
library(dplyr)

source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
source(file.path(getwd(), "Filter_development/Visual_Filter_DB_establishment/check_and_clean_duplicates_in_localizations.R"))
source(file.path(getwd(), "atlas_metrics.R"))
source(file.path(getwd(), "Filter_development", "Feature_engineering", "calculate_point_based_features.R"))
source(file.path(getwd(),"Filter_development/Feature_engineering/calculate_time_window_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_post_window_features.R"))
source(file.path(getwd(), "save_ATLAS_data_to_sqlite.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB"
path_to_species_metadata <- file.path(path_to_db, "Species_metadata.xlsx")
folder_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"

# Define the time gap between tracks in seconds. 
# This is the time gap that most likely distinguished between different trakectories of the same animal.
gap_between_tracks_sec <- 600 

half_time_window_size_sec <- 25

### USER INPUT END

# Load species metadata
species_metadata <- read_excel(path_to_species_metadata)

# all_data <- load_atlas_data_from_sqlite(file.path(path_to_db, "labeled_data_db.sqlite"))

# Run on the species
for (species_id in species_metadata$Species_ID) {
  
  print(species_id)
  
  # Set the soecies' sqlite file name and path
  file_name <- paste0(species_id, "_labeled_data.sqlite")
  file_path <- file.path(path_to_db, "Combined_species_data", file_name)
  
  # Load the species data from the sqlite file
  data <- load_atlas_data_from_sqlite(file_path)
  
  # Extract the localizations and detections data
  localization_data <- data$LOCALIZATIONS
  detection_data <- data$DETECTIONS
  
  # Delete duplicates from the localizations data- in case there are duplicates
  localization_data <- check_and_clean_duplicates_in_localizations(
    localization_data = localization_data,
    clean_duplicates = TRUE)
  
  # Convert dateTime to a human-readable format- relevant only for the data I got by March 2025
  localization_data$dateTime <- as.POSIXct(localization_data$dateTime, origin = "1970-01-01", tz = "UTC")
  
  # Calculate the time difference between consecutive points
  localization_data$time_diff_sec <- calculate_time_diff(localization_data$TIME)
  
  # For each individual (tag number), evaluate the track_id
  # A new track starts if the time difference  > gap_between_tracks_sec
  localization_data <- localization_data %>%
    arrange(TAG, TIME) %>%
    group_by(TAG) %>%
    mutate(
      new_track = if_else(is.na(time_diff_sec) | time_diff_sec > gap_between_tracks_sec, 1, 0),
      track_id = cumsum(new_track)
    ) %>%
    ungroup()
  
  # Calculate the point-based_features
  localization_data <- calculate_point_based_features(localization_data, detection_data)

  # Calculate the time-window-based features
  localization_data <- calculate_time_window_based_features(localizations_data = localization_data,
                                                            half_window_size_sec = half_time_window_size_sec)

  # Calculate features that require values of other point-based and window-based features
  localization_data <- calculate_post_window_features(localization_data)
  
  # Print the first rows of localization_data
  print(head(localization_data))
  
  ## Save the data as sqlite
  output_file_name <- paste0(species_id, "_features_eng.sqlite")
  save_ATLAS_data_to_sqlite(localizations_data = localization_data,
                            detections_data = detection_data,
                            fullpath = file.path(folder_to_save_results, output_file_name))
}
