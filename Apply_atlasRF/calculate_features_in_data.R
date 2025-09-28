
library(dplyr)
library(data.table)

source(file.path(getwd(), "Build_atlasRF/Feature_engineering", "calculate_point_based_features.R"))
source(file.path(getwd(), "Build_atlasRF/Feature_engineering/calculate_detection_based_features.R"))
source(file.path(getwd(), "Build_atlasRF/Feature_engineering/calculate_abs_avg_elevation_diff_between_location_and_participating_bs.R"))
source(file.path(getwd(), "Build_atlasRF/Feature_engineering/calculate_beacon_derived_features.R"))
source(file.path(getwd(), "Build_atlasRF/Feature_engineering/calculate_time_window_based_features.R"))
source(file.path(getwd(), "Build_atlasRF/Feature_engineering/calculate_post_window_features.R"))


#' @title Calculate Features for ATLAS Localization Data
#'
#' @description
#' This function orchestrates the feature engineering pipeline for ATLAS animal 
#' movement data. It integrates multiple feature sets including point-based, 
#' detection-based, elevation-related, beacon-derived, time-window-based, and 
#' post-window features into the localization dataset.
#'
#' @param localization_data data.frame or data.table  
#'   Localization data containing estimated tag positions.  
#' @param detection_data data.frame or data.table  
#'   Raw detection-level data for the same tags, used to calculate 
#'   detection-based features.  
#' @param base_stations_info data.frame or data.table  
#'   Metadata about base stations (e.g., location, elevation).  
#' @param beacons_detection_ratio_per_hour data.frame or data.table  
#'   Hourly detection ratios for reference beacons, used in beacon-derived 
#'   feature calculations.  
#' @param base_stations_summary_per_beacon data.frame or data.table  
#'   Summary statistics of base station performance per beacon.  
#' @param low_beacon_detection_fraction numeric  
#'   Threshold for flagging beacons with low detection fractions.  
#' @param half_time_window_size_sec numeric  
#'   Half-size of the time window (in seconds) used for time-window-based 
#'   feature calculations.  
#'
#' @return data.table  
#'   The input `localization_data` with additional feature columns.  
#'
#' @details
#' Internally, this function calls:
#' - `calculate_point_based_features()`  
#' - `calculate_detection_based_features()`  
#' - `calculate_abs_avg_elevation_diff_between_location_and_participating_bs()`  
#' - `calculate_beacon_derived_features()`  
#' - `calculate_time_window_based_features()`  
#' - `calculate_post_window_features()`  
#'
#' These enrich the localization data with features related to geometry, 
#' base-station participation, beacon reliability, elevation mismatches, 
#' temporal context, and higher-order dependencies.
#'
#' @note  
#' The current implementation references external objects 
#' (`feature_calculation_settings` and `config$feature_calculation_settings`).  
#' For cleaner function encapsulation, consider replacing these with the explicit 
#' function arguments `low_beacon_detection_fraction` and 
#' `half_time_window_size_sec`.  
#'
#' @export
#'
#' @examples
#' \dontrun{
#' features <- calculate_features_in_data(localization_data, detection_data,
#'                                        base_stations_info,
#'                                        beacons_detection_ratio_per_hour,
#'                                        base_stations_summary_per_beacon,
#'                                        low_beacon_detection_fraction = 0.1,
#'                                        half_time_window_size_sec = 60)
#' }
#' 
calculate_features_in_data <- function(localization_data,
                                       detection_data,
                                       base_stations_info,
                                       beacons_detection_ratio_per_hour,
                                       base_stations_summary_per_beacon,
                                       low_beacon_detection_fraction,
                                       half_time_window_size_sec) {
  
  message("Calculating features:")
  
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
                                                         config$feature_calculation_settings$low_beacon_detection_fraction)
  
  # Calculate the time-window-based features
  localization_data <- calculate_time_window_based_features(localization_data = localization_data,
                                                            half_window_size_sec = config$feature_calculation_settings$half_time_window_size_sec)
  
  # Calculate features that require values of other point-based and window-based features
  localization_data <- calculate_post_window_features(localization_data)
  
  return(localization_data)
  
}