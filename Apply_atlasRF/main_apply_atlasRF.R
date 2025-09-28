
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

# Load the config file
source(file.path(getwd(), "Apply_atlasRF/config_apply_atlasRF.R"))

# Load the other functions that do each step of the atlasRF filter
source(file.path(getwd(), "Apply_atlasRF/wrapper_retrieve_raw_atlas_data.R"))
source(file.path(getwd(), "Apply_atlasRF/wrapper_add_lat_lon_coordinates_to_localization_data.R"))
source(file.path(getwd(), "Apply_atlasRF/wrapper_calculate_features.R"))
source(file.path(getwd(), "Apply_atlasRF/clean_feature_data.R"))
source(file.path(getwd(), "Apply_atlasRF/wrapper_run_atlasRF.R"))
source(file.path(getwd(), "Apply_atlasRF/clean_atlasRF_results.R"))
source(file.path(getwd(), "Apply_atlasRF/wrapper_save_final_atlasRF_results.R"))

###

# # Install the required R packages- if not yet installed
# source(file.path(getwd(),""ATLAS_data_retrieval/install_required_R_packages.R"))

### Create a results folder- if doesn't yet exist
if (!dir.exists(config$paths$folder_atlasRF_results)) {
  dir.create(config$paths$folder_atlasRF_results, recursive = TRUE)
}

### load unfiltered data from the ATLAS database
raw_atlas_data <- wrapper_retrieve_raw_atlas_data(config)
raw_localization_data <- raw_atlas_data$LOCALIZATIONS
raw_detection_data <- raw_atlas_data$DETECTIONS

### Add latitude and longitude coordinates to the localization data
raw_localization_data <- wrapper_add_lat_lon_coordinates_to_localization_data(
  raw_localization_data, config$atlas_time_and_coordinates_info$atlas_data_crs)

### Calculate features required for the atlasRF filter
localization_data_with_features <- wrapper_calculate_features(
  config, raw_localization_data, raw_detection_data)

### Clean and prepare the feature data for atlasRF
cleaned_data <- clean_feature_data(
  localization_data_with_features, config$random_forest_settings$non_feature_column_names)

### Run atlasRF on the cleaned data
localization_data_with_outlier_labels <- wrapper_run_atlasRF(
  cleaned_data, config)

### Clean the atlasRF results (remove Outliers == NA)
localization_data_with_outlier_labels <- clean_atlasRF_results(localization_data_with_outlier_labels)

### **OPTIONAL** Compare the atlasRF outlier labeling to the Confidence Filter 
if (config$confidence_filter_settings$compare_atlasRF_to_confidence_filter) {
  
  source(file.path(getwd(), "Apply_atlasRF/wrapper_compare_atlasRF_and_confidence_filter.R"))
  localization_data_with_outlier_labels <- wrapper_compare_atlasRF_and_confidence_filter(localization_data_with_outlier_labels, config)

}

### Save the final localization data with the outlier labels
wrapper_save_final_atlasRF_results(localization_data_with_outlier_labels, config)

message("*** atlasRF applying process is finished! ***")