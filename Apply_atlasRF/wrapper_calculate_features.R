
source(file.path(getwd(), "get_global_time_range_of_data_requests.R"))
source(file.path(getwd(), "create_filename_without_extension.R"))
source(file.path(getwd(), "Build_atlasRF/Feature_engineering/load_and_format_base_stations_info.R"))
source(file.path(getwd(), "Apply_atlasRF/calculate_features_in_data.R"))

#' @title Wrapper to Calculate Features for ATLAS Data
#'
#' @description
#' High-level wrapper that manages feature calculation for ATLAS localization and 
#' detection data. It determines the global time range of data requests, builds a 
#' consistent filename, loads required auxiliary information (base stations and 
#' beacon tables), checks for cached results, and either loads existing features 
#' from disk or computes new ones using `calculate_features_in_data()`.
#'
#' @param config list  
#'   Configuration list containing all relevant paths, settings, and metadata.  
#'   Expected fields:  
#'   - `data_requests`: list of requested tag/time combinations  
#'   - `atlas_time_and_coordinates_info`: list with `atlas_time_zone`, 
#'     `atlas_time_format`  
#'   - `animal_name_code`: identifier for the animal  
#'   - `paths`: list of paths, including  
#'       - `folder_atlasRF_results`  
#'       - `base_stations_info_path`  
#'       - `folder_of_beacons_info_tables`  
#'       - `filename_base_stations_summary_per_beacon`  
#'       - `filename_beacons_detection_ratio_table`  
#'   - `feature_calculation_settings`: list with  
#'       - `low_beacon_detection_fraction`  
#'       - `half_time_window_size_sec`  
#'
#' @param raw_localization_data data.frame or data.table  
#'   Raw localization data containing estimated positions.  
#' @param raw_detection_data data.frame or data.table  
#'   Raw detection data used for detection-based feature calculations.  
#'
#' @return  
#'   A localization dataset augmented with calculated feature columns.  
#'   If a cached file already exists, the function loads and returns it.  
#'
#' @details
#' Workflow:  
#' 1. Determine global time range across all requested tags (`get_global_time_range_of_data_requests()`).  
#' 2. Construct a standardized filename (`create_filename_without_extension()`).  
#' 3. Check if the feature-enriched dataset already exists in 
#'    `config$paths$folder_atlasRF_results`.  
#' 4. If so, load it via `readRDS()`. Otherwise:  
#'    - Load base station metadata (`load_and_format_base_stations_info()`).  
#'    - Load beacon summaries and detection ratios.  
#'    - Call `calculate_features_in_data()` to compute features.  
#'    - Save the result for future reuse with `saveRDS()`.  
#'
#' The function prints progress messages and a preview of the processed 
#' localization data.  
#'
#' @export
#'
#' @examples
#' \dontrun{
#' features <- wrapper_calculate_features(
#'   config = my_config,
#'   raw_localization_data = locs,
#'   raw_detection_data = dets
#' )
#' }
#' 
wrapper_calculate_features <- function(config, raw_localization_data, raw_detection_data) {
  
  message("*** Started features calculation. ***")
  
  # Get the global start and end times of all the data
  global_times_data <- get_global_time_range_of_data_requests(
    data_requests = config$data_requests,
    data_time_zone = config$atlas_time_and_coordinates_info$atlas_time_zone,
    data_time_format = config$atlas_time_and_coordinates_info$atlas_time_format)
  
  # Generate the file name without extension- to save the ATLAS data
  filename_without_extension <- create_filename_without_extension(
    animal_name_code = config$animal_name_code, 
    tag_numbers = sapply(config$data_requests, function(x) x$tag), 
    start_time = global_times_data$start_time, 
    end_time = global_times_data$end_time)
  
  # Generate the full path of the data with features' file
  fullpath_to_rds_file_with_features <- file.path(
    config$paths$folder_atlasRF_results, paste0(filename_without_extension, "_with_features.rds"))
  
  # Check if the features' file already exists
  if (file.exists(fullpath_to_rds_file_with_features)) {
    
    message("Loading localization data with features from file.")
    localization_data_with_features <- readRDS(fullpath_to_rds_file_with_features)
    
  } else {
    
    ## Load the base stations info
    base_stations_info <- load_and_format_base_stations_info(config$paths$base_stations_info_path)
    
    ## Load the beacons data
    # Load the hourly detections counts of the beacons
    base_stations_summary_per_beacon <- readRDS(file.path(config$paths$folder_of_beacons_info_tables, config$paths$filename_base_stations_summary_per_beacon))
    # Load the beacons detection ratio table
    beacons_detection_ratio_per_hour <- readRDS(file.path(config$paths$folder_of_beacons_info_tables, config$paths$filename_beacons_detection_ratio_table))
    
    ## Calculate the required features for atlasRF
    localization_data_with_features <- calculate_features_in_data(
      raw_localization_data,
      raw_detection_data,
      base_stations_info,
      beacons_detection_ratio_per_hour,
      base_stations_summary_per_beacon,
      config$feature_calculation_settings$low_beacon_detection_fraction,
      config$feature_calculation_settings$half_time_window_size_sec)
    
    # # Add the species_id to the data tables
    # localization_data$Species_id <- species_id
    # detection_data$Species_id <- species_id
    # participating_base_stations$Species_id <- species_id
    # missed_base_stations$Species_id <- species_id
    
    # Print the first rows of localization_data
    print("Final LOCALIZATIONS with features:")
    # print(colnames(localization_data))
    print(head(localization_data_with_features, 20))
    # print(localization_data)
    # print(colSums(is.na(localization_data)))  # Number of NA per column
    # print(ncol(localization_data))
    
    # Save the data with features for future use
    saveRDS(localization_data_with_features, file = fullpath_to_rds_file_with_features)
    message("Saved localization data with features.")
  }
  
  return(localization_data_with_features)
  
}

