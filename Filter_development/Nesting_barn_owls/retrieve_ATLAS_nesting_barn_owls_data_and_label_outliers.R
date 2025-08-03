# This script runs the entire routine to get the ATLAS data and filter them
# ONE TAG AT A TIME- CAN BE EXTENDED FOR MULTIPLE TAGS IF NOT USED LATER WITH THE VISUAL FILTER APP

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

library(dplyr)
library(ggplot2)

source(file.path(getwd(), "Filter_development/Nesting_barn_owls/config_nesting_barn_owls.R"))
source(file.path(getwd(), "create_filename_without_extension.R"))
source(file.path(getwd(), "calculate_lat_lon_coordinates.R"))
source(file.path(getwd(), "ATLAS_data_retrieval/calculate_features_in_data.R"))
source(file.path(getwd(), "Filter_development/Nesting_barn_owls/label_outleirs_by_dist_from_nest_box.R"))

###

# # Install the required R packages- if not yet installed
# source(file.path(getwd(),""ATLAS_data_retrieval/install_required_R_packages.R"))

### Load required files

# Load the hourly detections counts of the beacons
base_stations_summary_per_beacon <- readRDS(file.path(folder_of_beacons_info_tables, filename_base_stations_summary_per_beacon))

# Load the beacons detection ratio table
beacons_detection_ratio_per_hour <- readRDS(file.path(folder_of_beacons_info_tables, filename_beacons_detection_ratio_table))

# Load the nesting barn owls metadata
nesting_bo_metadata <- read.csv(path_to_nesting_bo_metadata)

### Load ATLAS data ###

# Generate the file name without extension- to save the ATLAS data
filename_without_extension <- create_filename_without_extension(
  animal_name_code = species_id, 
  tag_numbers = tag_number, 
  start_time = start_time, 
  end_time = end_time)

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

data_request <- list(
  list(animal_name_code = species_id, 
       tag = tag_number, 
       start_time = start_time, 
       end_time = end_time)
)

# Get the ATLAS data- either from the server, or from an SQLite file
source(file.path(getwd(),"get_ATLAS_data.R"))
raw_atlas_data = get_ATLAS_data(data_requests = data_request, 
                                atlas_db_credentials = harod_db_credentials,
                                retrieve_data_from_server = retrieve_data_from_server,
                                save_data_to_sqlite_file = save_data_to_sqlite_file,
                                full_paths_to_sqlite_files = fullpath_to_sqlite_file)

raw_localization_data <- raw_atlas_data$LOCALIZATIONS
raw_detection_data <- raw_atlas_data$DETECTIONS

## Prepare data for feature calculation ##

# Calculate 'lat' and 'lon' coordinates
raw_localization_data <- calculate_lat_lon_coordinates(raw_localization_data, data_crs)

# Convert raw_localization_data from sf object back to a dataframe
raw_localization_data <- as.data.frame(raw_localization_data)

# Add column Species_id
raw_localization_data$Species_id <- species_id

## Calculate features ##

# Calculate features required for the atlasRF filter- or load from existing file
fullpath_to_rds_file_with_features <- file.path(folder_to_save_daa_with_features, paste0(filename_without_extension, "_with_features.rds"))

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

## Label outliers by threshold distance from the nesting box ##
localization_data_with_outliers <- label_outleirs_by_dist_from_nest_box(
  localization_data_with_features, 
  nesting_bo_metadata, 
  threshold_dist_from_nest_m)

# Save the labeled data as rds
saveRDS(localization_data_with_outliers, file.path(folder_to_save_labeled_data, paste0(filename_without_extension, "_labeled.rds")))
message("Saved localization data with outliers labeling.")

## Plot histograms of Outlier labels for each feature ##
message("Generating and saving histograms of outlier labels per feature:")

# Remove non-feature columns
localization_data_only_features <- localization_data_with_outliers[, !names(localization_data_with_outliers) %in% non_feature_column_names]

# Convert Outliers to factor (optional, for consistent coloring)
localization_data_only_features$Outliers <- factor(
  localization_data_only_features$Outliers,
  levels = c(0, 1),
  labels = c("valid", "outlier")
)

# Plot histograms of outlier labels and save as png
# Get the feature columns (excluding the Outliers column)
feature_columns <- setdiff(names(localization_data_only_features), "Outliers")

# Loop through each feature and create the histogram
for (feature_name in feature_columns) {
  
  print(feature_name)
  
  # Only plot numeric columns
  if (is.numeric(localization_data_only_features[[feature_name]])) {
    
    # Create histogram with ggplot
    p <- ggplot(localization_data_only_features, aes(x = .data[[feature_name]], fill = Outliers)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
      scale_fill_manual(values = c("valid" = "green", "outlier" = "red")) +
      theme_minimal() +
      labs(title = paste("Histogram of", feature_name),
           x = feature_name,
           y = "Count") +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Save plot
    ggsave(filename = file.path(path_to_save_feature_histograms, paste0(feature_name, ".png")),
           plot = p,
           width = 8, height = 6, bg = "white")
  }
}

# 

