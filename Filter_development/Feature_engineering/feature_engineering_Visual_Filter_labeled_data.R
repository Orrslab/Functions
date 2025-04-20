# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(readxl)
library(dplyr)

source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
source(file.path(getwd(), "atlas_metrics.R"))
source(file.path(getwd(), "Filter_development", "Feature_engineering", "calculate_point_based_features.R"))
source(file.path(getwd(),"Filter_development/Feature_engineering/calculate_time_window_based_features.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB"
path_to_species_metadata <- file.path(path_to_db, "Species_metadata.xlsx")
path_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering"

# Define the time gap between tracks in seconds. 
# This is the time gap that most likely distinguished between different trakectories of the same animal.
gap_between_tracks_sec <- 600 

half_time_window_size_sec <- 20

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
  
}




################################
# # Load the features' info
# load(file.path(getwd(), "Filter_development", "Feature_engineering", "features_info.RData"))
# 
# # Load species metadata
# species_metadata <- read_excel(path_to_species_metadata)
# 
# for (i in 1:nrow(species_metadata)) {
#   
#   species_id <- species_metadata$Species_ID[i]
#   species_name <- species_metadata$Species_name[i]
#   
#   message(paste("Features evaluation for", species_id, "-", species_name))
#   
#   # Set path to save the results of the species
#   path_to_save_results_species <- file.path(path_to_save_results, species_id)
#   
#   # Create the results path if it does not yet exist
#   if (!dir.exists(path_to_save_results_species)) {
#     # If it doesn't exist, create it
#     dir.create(path_to_save_results_species, recursive = TRUE)  # recursive = TRUE ensures that all parent directories are created as needed
#   }
#   
#   ### Load the locations and detections data from the species file
#   
#   # Set the file name and path
#   species_data_file_name <- paste0(species_id, "_localizations_annotated.sqlite")
#   species_data_path <- file.path(path_to_data, species_data_file_name)
#   
#   # Load the labeled data
#   # TODO Add DETECITIONS to the combined data
#   # source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
#   # species_data <- load_atlas_data_from_sqlite(species_data_path)
#   source(file.path(getwd(), "load_localizations_data_from_sqlite_file.R"))
#   species_data <- load_localizations_data_from_sqlite_file(species_data_path)
#   
#   ### Feature Engineering
#   source(file.path(getwd(), "atlas_metrics.R"))
#   
#   # Cosine of the turning angle
#   species_data$cos_turning_angle <- calculate_cosine_turning_angle(X_column = species_data$X,
#                                                                    Y_column = species_data$Y)
#   
#   # ### Save into sqlite the labeled data with all the added features
#   # source(paste0(path_to_functions, "save_ATLAS_data_to_sqlite.R"))
#   # fullpath <- paste0(data_folder, "/labeled_data_with_features.sqlite")
#   # save_ATLAS_data_to_sqlite(localizations_data=location_data_labeled, 
#   #                           fullpath = fullpath)
#   
#   ### Evaluate outliers proportion
#   outliers_proportion_table <- table(species_data$Outliers)
#   filename_outliers_proportion <- "outliers_proportion.csv"
#   write.csv(outliers_proportion_table, file = file.path(path_to_save_results_species, filename_outliers_proportion), row.names = FALSE)
#   
#   ### Data Statistics and Visualization
#   source(file.path(getwd(), "Filter_development", "Feature_engineering", "plot_exploratory_analysis.R"))
#   for (feature_info in features_info) {
#     print(feature_info$feature_name)
#     plot_exploratory_analysis(species_data = species_data,
#                               feature_column_name = feature_info$feature_column_name,
#                               feature_name = feature_info$feature_name,
#                               feature_units = feature_info$feature_units)
#   }
#   
# }
# 
