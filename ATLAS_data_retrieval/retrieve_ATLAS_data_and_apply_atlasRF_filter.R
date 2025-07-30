# This script runs the entire routine to get the ATLAS data and filter them
# ONE TAG AT A TIME- CAN BE EXTENDED FOR MULTIPLE TAGS IF NOT USED LATER WITH THE VISUAL FILTER APP

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

source(file.path(getwd(), "ATLAS_data_retrieval/config_atlasRF.R"))
source(file.path(getwd(), "ATLAS_data_retrieval", data_requests_file_name))
source(file.path(getwd(), "create_filename_without_extension.R"))
source(file.path(getwd(), "calculate_lat_lon_coordinates.R"))
source(file.path(getwd(), "ATLAS_data_retrieval/calculate_features_in_data.R"))
source(file.path(getwd(), "ATLAS_data_retrieval/run_rf_on_localization_data.R"))

###

# # Install the required R packages- if not yet installed
# source(file.path(getwd(),""ATLAS_data_retrieval/install_required_R_packages.R"))

### Load required files

# Load the hourly detections counts of the beacons
base_stations_summary_per_beacon <- readRDS(file.path(folder_of_beacons_info_tables, filename_base_stations_summary_per_beacon))

# Load the beacons detection ratio table
beacons_detection_ratio_per_hour <- readRDS(file.path(folder_of_beacons_info_tables, filename_beacons_detection_ratio_table))

# Load the atlasRF model
rf_model <- readRDS(file.path(random_forest_model_folder, random_forest_model_filename))

### Load ATLAS data ###

animal_metadata <- data_requests[[1]]

# Generate the file name without extension- to save the ATLAS data
filename_without_extension <- create_filename_without_extension(
  animal_name_code = animal_metadata$animal_name_code, 
  tag_numbers = animal_metadata$tag, 
  start_time = animal_metadata$start_time, 
  end_time = animal_metadata$end_time)

# Set the folder path to save or retrieve the atlas data as sqlite
if (retrieve_data_from_server) {
  folder_path_to_atlas_files <- fodler_path_to_save_sqlite_files
} else {
  folder_path_to_atlas_files <- fodler_path_to_retrieve_sqlite_files
}

fullpath_to_sqlite_file <- file.path(folder_path_to_atlas_files, paste0(filename_without_extension, ".sqlite"))

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
                                full_paths_to_sqlite_files = fullpath_to_sqlite_file)

raw_localization_data <- raw_atlas_data$LOCALIZATIONS
raw_detection_data <- raw_atlas_data$DETECTIONS

# Calculate 'lat' and 'lon' coordinates
raw_localization_data <- calculate_lat_lon_coordinates(raw_localization_data, data_crs)

# Convert raw_localization_data from sf object back to a dataframe
raw_localization_data <- as.data.frame(raw_localization_data)

### Apply the atlasRF Filter ###

# Calculate features required for the atlasRF filter- or load from existing file
fullpath_to_rds_file_with_features <- file.path(folder_path_to_atlas_files, paste0(filename_without_extension, "_with_features.rds"))

if (file.exists(fullpath_to_rds_file_with_features)) {
  message("Loading localization data with features from file.")
  localization_data_with_features <- readRDS(fullpath_to_rds_file_with_features)
} else {
  # Calculate the required features
  localization_data_with_features <- calculate_features_in_data(
    raw_localization_data,
    raw_detection_data,
    base_stations_info_path,
    beacons_detection_ratio_per_hour,
    base_stations_summary_per_beacon,
    low_beacon_detection_fraction,
    half_time_window_size_sec)

  # Save the data with features for future use
  saveRDS(localization_data_with_features, file = fullpath_to_rds_file_with_features)
  message("Saved localization data with features.")
}

# Run the Random Forest model on the localization data with the features
localization_data_with_outliers <- run_rf_on_localization_data(localization_data_with_features,
                                                               non_feature_column_names,
                                                               rf_model,
                                                               rf_prediction_threshold)

# Convert Outliers column from factor to binary to use on the Visual Filter App
localization_data_with_outliers$Outliers <- ifelse(localization_data_with_outliers$Outliers == "outlier", 1, 0)

# Save the localization data with the Outliers predictions as rds
saveRDS(localization_data_with_outliers, file = file.path(folder_to_save_labeled_data, paste0(filename_without_extension, "_with_outlier_labels.rds")))

message("Saved localization data with Outlier labels.")

### Apply the Confidence Filter ###

# Calculate the confidence of each location point
source(file.path(getwd(),"Track_cpp.R"))
localization_data_with_outliers <- TrackConfidenceLevelcpp(raw_localization_data,
                                                           conectedVel=13,
                                                           conectedDist=NA,
                                                           stdlim=80,
                                                           minNBSforConf2=7,
                                                           minNBSforConf1=4,
                                                           Nconf1forConf2=5)

# If Conf == 2 then Outliers = 0, else Outliers = 1
localization_data_with_outliers$Outliers <- ifelse(localization_data_with_outliers$Conf == 2, 0, 1)

# Save the localization data with the Outliers predictions as rds
saveRDS(localization_data_with_outliers, file = file.path(folder_to_save_labeled_data, paste0(filename_without_extension, "_with_outlier_labels_confidence.rds")))

message("Saved localization data with Outlier labels.")

###

# Activate the shiny Visual Filter
# source(paste0(path_to_atlas_data_analysis_repo, "visual_filter_shiny_app.R"))