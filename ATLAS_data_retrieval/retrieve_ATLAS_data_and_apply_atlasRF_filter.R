# This script runs the entire routine to get the ATLAS data and filter them

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

source(file.path(getwd(), "ATLAS_data_retrieval/config_atlasRF.R"))
source(file.path(getwd(), "calculate_lat_lon_coordinates.R"))
source(file.path(getwd(), "ATLAS_data_retrieval/calculate_features_in_data.R"))
source(file.path(getwd(), "ATLAS_data_retrieval/run_rf_on_localization_data.R"))

###

# # Install the required R packages- if not yet installed
# source(file.path(path_to_scripts,"install_required_R_packages.R"))


## Load required files

# Load the hourly detections counts of the beacons
base_stations_summary_per_beacon <- readRDS(file.path(folder_of_beacons_info_tables, filename_base_stations_summary_per_beacon))

# Load the beacons detection ratio table
beacons_detection_ratio_per_hour <- readRDS(file.path(folder_of_beacons_info_tables, filename_beacons_detection_ratio_table))

# Load the atlasRF model
rf_model <- readRDS(file.path(random_forest_model_folder, random_forest_model_filename))
##

# Get the desired tag numbers and date ranges for the data retrieval
source(file.path(path_to_scripts, data_requests_file_name))

# Set the folder path to save or retrieve the atlas data sqlite files 
if (retrieve_data_from_server) {
  folder_path_to_sqlite_files <- fodler_path_to_save_sqlite_files
} else {
  folder_path_to_sqlite_files <- fodler_path_to_retrieve_sqlite_files
}

# Generate the file names from the tag numbers and dates
source(file.path(getwd(), "create_list_of_sqlite_filepaths.R"))
fullpaths_to_sqlite_files <- create_list_of_sqlite_filepaths(data_requests, 
                                                             folder_path_to_sqlite_files)

# Get the ATLAS database credentials from the config file
harod_db_credentials <- list(
  system_name = system_name_harod,         # System name
  db_username = db_username_harod,         # username
  db_pass = db_pass_harod,                 # password
  db_host_ip = db_host_ip_harod,           # host IP address
  db_port_number = db_port_number_harod,   # port number
  db_name = db_name_harod                  # database name
)

# Get the ATLAS data- either from the server, or from an SQLite file
source(file.path(getwd(),"get_ATLAS_data.R"))
raw_atlas_data = get_ATLAS_data(data_requests = data_requests, 
                                atlas_db_credentials = harod_db_credentials,
                                retrieve_data_from_server = retrieve_data_from_server,
                                save_data_to_sqlite_file = save_data_to_sqlite_file,
                                full_paths_to_sqlite_files = fullpaths_to_sqlite_files)

raw_localization_data <- raw_atlas_data$LOCALIZATIONS
raw_detection_data <- raw_atlas_data$DETECTIONS

# Calculate 'lat' and 'lon' coordinates
raw_localization_data <- calculate_lat_lon_coordinates(raw_localization_data, data_crs)

# Convert raw_localization_data from sf object back to a dataframe
raw_localization_data <- as.data.frame(raw_localization_data)

# Calculate features required for the atlasRF filter
localization_data_with_features <- calculate_features_in_data(
  raw_localization_data,
  raw_detection_data,
  base_stations_info_path,
  beacons_detection_ratio_per_hour,
  base_stations_summary_per_beacon,
  low_beacon_detection_fraction,
  half_time_window_size_sec)

# Run the Random Forest model on the localization data with the features
localization_data_with_outliers <- run_rf_on_localization_data(localization_data_with_features, 
                                                               non_feature_column_names, 
                                                               rf_model,
                                                               rf_prediction_threshold)

# Convert Outliers column from factor to binary to use on the Visual Filter App
localization_data_with_outliers$Outliers <- ifelse(localization_data_with_outliers$Outliers == "outlier", 1, 0)

# Save the localization data with the Outliers predictions as rds
saveRDS(localization_data_with_outliers, file = file.path(folder_to_save_labeled_data, paste0(labeled_data_filename_without_extension, ".rds")))

message("Saved localization data with Outlier labels.")

# # Calculate the confidence of each location point
# source(paste0(path_to_atlas_data_analysis_repo,"Track_cpp.R"))
# raw_data_with_confidence_levels <- TrackConfidenceLevelcpp(raw_location_data,
#                                                            conectedVel=20,
#                                                            conectedDist=NA,
#                                                            stdlim=80,
#                                                            minNBSforConf2=7,
#                                                            minNBSforConf1=4,
#                                                            Nconf1forConf2=5)
# 
# # Filter all data with conf = 2
# data__with_confidence_2 <- raw_data_with_confidence_levels %>%
#   filter(Conf == 2)

# # Save the raw data to sqlite
# source(paste0(path_to_atlas_data_analysis_repo, "save_ATLAS_data_to_sqlite.R"))
# save_ATLAS_data_to_sqlite(localization_data = raw_location_data, 
#                           tag_number = 972006000836, 
#                           start_time = '2023-12-24 00:00:01',
#                           end_time = '2023-12-25 00:00:01')

# Activate the shiny Visual Filter
# source(paste0(path_to_atlas_data_analysis_repo, "visual_filter_shiny_app.R"))