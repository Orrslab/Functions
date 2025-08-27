
library(dplyr)
library(data.table)

source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
source(file.path(getwd(), "Filter_development", "Feature_engineering", "calculate_point_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/load_and_format_base_stations_info.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_abs_avg_elevation_diff_between_location_and_participating_bs.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_detection_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_beacon_derived_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_time_window_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_post_window_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/save_ATLAS_data_with_features_to_sqlite.R"))

#' Main pipeline for calculating ATLAS localization features
#'
#' This function orchestrates the full feature engineering pipeline for 
#' ATLAS tracking data, looping over species and calculating multiple 
#' groups of features. It integrates movement, detection, base station, 
#' beacon-derived, and time-window-based metrics, and saves the results 
#' into per-species SQLite databases.
#'
#' @param config A configuration object (list) containing required paths 
#' and settings. Must include:
#' \describe{
#'   \item{paths}{A list of file paths and folders, including: 
#'     \code{species_metadata_file_path}, 
#'     \code{base_stations_info_path},
#'     \code{folder_of_beacons_info_tables}, 
#'     \code{filename_base_stations_summary_per_beacon},
#'     \code{filename_beacons_detection_ratio_table},
#'     \code{combined_species_data_folder},
#'     \code{folder_to_save_features_results}.}
#'   \item{feature_settings}{A list of feature calculation parameters, 
#'     including: 
#'     \code{low_beacon_detection_fraction} (numeric threshold for 
#'     low detection), 
#'     \code{half_time_window_size_sec} (integer, half window size in seconds).}
#' }
#'
#' @details 
#' For each species listed in the metadata, the function:
#' \enumerate{
#'   \item Loads species-specific localizations and detections from SQLite.
#'   \item Converts timestamps to POSIXct format.
#'   \item Calculates point-based features (movement, geometry, error, elevation).
#'   \item Calculates detection-based features (per base station).
#'   \item Derives elevation difference relative to participating base stations.
#'   \item Calculates beacon-derived features using beacon detection ratios and summaries.
#'   \item Adds time-window-based features centered on each localization.
#'   \item Calculates post-window features dependent on earlier metrics.
#'   \item Appends species IDs to all tables.
#'   \item Saves results into a species-specific SQLite database.
#' }
#'
#' @return 
#' The function has no direct return value. It produces SQLite databases in 
#' \code{config$paths$folder_to_save_features_results}, one per species, 
#' with enriched localization, detection, and metadata tables.
#'
#' @note 
#' This function is intended as the entry point for feature engineering 
#' in the atlasRF pipeline. Intermediate data frames are printed or checked 
#' only for debugging (e.g. counts of missing values).
#'
#' @import dplyr data.table
#' @export
#' 
main_calculate_features <- function(config) {
  
  message("### STARTED THE MAIN SCRIPT OF THE FEATURES CALCULATION. ###")
  
  # Load species metadata
  species_metadata <- read.csv(config$paths$species_metadata_file_path)
  
  # Load the base stations info
  base_stations_info <- load_and_format_base_stations_info(config$paths$base_stations_info_path)
  
  # Load the hourly detections counts of the beacons
  base_stations_summary_per_beacon <- readRDS(file.path(config$paths$folder_of_beacons_info_tables, config$paths$filename_base_stations_summary_per_beacon))
  
  # Load the beacons detection ratio table
  beacons_detection_ratio_per_hour <- readRDS(file.path(config$paths$folder_of_beacons_info_tables, config$paths$filename_beacons_detection_ratio_table))
  
  # all_data <- load_atlas_data_from_sqlite(file.path(config$paths$path_to_db, "labeled_data_db.sqlite"))
  
  # Run on the species
  for (species_id in species_metadata$Species_id) {
    
    # For debug purposes
    # species_id <- "LD"
    # species_id <- "CB"
    # species_id <- "RW"
    # species_id <- "GJ"
    
    print(species_id)
    
    # Set the species' sqlite file name and path
    file_name <- paste0(species_id, "_labeled_data.sqlite")
    file_path <- file.path(config$paths$combined_species_data_folder, file_name)
    
    # Load the species data from the sqlite file
    data <- load_atlas_data_from_sqlite(file_path)
    
    # Extract the localizations and detections data
    localization_data <- data$LOCALIZATIONS
    detection_data <- data$DETECTIONS
    
    ### DEBUGGING
    # localization_data <- localization_data[1:5000, ]
    ###
    
    # Convert dateTime to a human-readable format
    localization_data$dateTime <- as.POSIXct(localization_data$dateTime, origin = "1970-01-01", tz = "UTC")
    
    # Calculate the point-based_features
    localization_data <- calculate_point_based_features(localization_data)
    
    # Calculate the detection-based features
    results <- calculate_detection_based_features(localization_data, 
                                                  detection_data,
                                                  base_stations_info)
    
    localization_data <- results$localization_data
    participating_base_stations <- results$participating_base_stations
    missed_base_stations <- results$missed_base_stations
    
    # Calculate the absolute value of the average difference between the location's elevation 
    # and the elevation of each participating base station
    localization_data <- calculate_abs_avg_elevation_diff_between_location_and_participating_bs(
      localization_data,
      participating_base_stations,
      base_stations_info
    )
    
    # Calculate beacons' features
    localization_data <- calculate_beacon_derived_features(localization_data, 
                                                           participating_base_stations, 
                                                           beacons_detection_ratio_per_hour,
                                                           base_stations_summary_per_beacon,
                                                           config$feature_settings$low_beacon_detection_fraction)
    
    # Calculate the time-window-based features
    localization_data <- calculate_time_window_based_features(localization_data = localization_data,
                                                              half_window_size_sec = config$feature_settings$half_time_window_size_sec)
    
    # Calculate features that require values of other point-based and window-based features
    localization_data <- calculate_post_window_features(localization_data)
    
    # Add the species_id to the data tables
    localization_data$Species_id <- species_id
    detection_data$Species_id <- species_id
    participating_base_stations$Species_id <- species_id
    missed_base_stations$Species_id <- species_id
    
    # Print the first rows of localization_data
    print("Final LOCALIZATIONS with features:")
    # print(colnames(localization_data))
    print(head(localization_data, 20))
    # print(localization_data)
    # print(colSums(is.na(localization_data)))  # Number of NA per column
    # print(ncol(localization_data))
    
    ## Save the data as sqlite
    output_file_name <- paste0(species_id, "_features_eng.sqlite")
    save_ATLAS_data_with_features_to_sqlite(
      localization_data = localization_data,
      detections_data = detection_data,
      participating_base_stations = participating_base_stations,
      missed_base_stations = missed_base_stations,
      fullpath = file.path(config$paths$folder_to_save_features_results, output_file_name))
  }
  
}
