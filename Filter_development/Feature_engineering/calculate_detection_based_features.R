#' Calculate detection-based features for each localization point
#'
#' This function enriches each localization in the dataset with features derived from the corresponding detections.
#' These features include signal-to-noise ratio (SNR) metrics, distance to the closest and farthest base stations, and metrics
#' related to missed base stations. It also returns a table of participating base stations for each localization.
#'
#' @param localizations_data A data frame containing the localizations, typically with columns such as `TAG`, `TIME`, `lon`, `lat`, etc.
#' @param detections_data A data frame containing the detection data, including all raw detections to be matched to localizations- this should be the raw detections data from the ATLAS database.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{localizations_data}{A data frame containing the original localization data enriched with the detection-based features.}
#'   \item{participating_base_stations}{A data frame indicating which base stations contributed to each localization.}
#'   \item{missed_base_stations}{A data frame or list indicating which base stations were expected but did not contribute to the localization.}
#' }
#'
#' @details
#' This function depends on multiple helper functions for:
#' \itemize{
#'   \item Matching detections to localizations
#'   \item Calculating SNR-related features
#'   \item Loading and formatting base station metadata
#'   \item Computing distance to the closest base station
#'   \item Identifying and analyzing missed base stations
#' }
#' These helper functions must be sourced before calling this function.
#'
#' @examples
#' \dontrun{
#' results <- calculate_detection_based_features(localizations_data, detections_data)
#' enriched_localization_data <- results$localizations_data
#' bs_table <- results$participating_base_stations
#' missed_bs <- results$missed_base_stations
#' }
#'
#' @export

source(file.path(getwd(), "match_detections_to_localizations.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_SNR_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/load_and_format_base_stations_info.R"))
source(file.path(getwd(), "create_participating_base_stations_table.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_distance_to_closest_and_farthest_base_station.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_missed_base_stations_features.R"))

# Features per location, that reqire knowing the detections that correspond to each location
calculate_detection_based_features <- function(localizations_data, detections_data) {
  
  # Get the detections corresponding to each location
  matched_detections <- match_detections_to_localizations(localizations_data, detections_data)
  
  # Calculate the SNR features
  localizations_data_with_features <- calculate_SNR_features(matched_detections, localizations_data)
  
  # Load the base stations info
  base_stations_info_path <- "C:/Users/netat/Documents/Movement_Ecology/ATLAS/Base_stations_beacons_info/Base_stations_info.csv"
  base_stations_info <- load_and_format_base_stations_info(base_stations_info_path)
  
  # Calculate the distance of each location from the closest base station
  localizations_data_with_features <- calculate_distance_to_closest_and_farthest_base_station(
    localizations_data_with_features,
    matched_detections,
    base_stations_info)
  
  # Create a table with the participating base stations in each localization
  participating_base_stations <- create_participating_base_stations_table(localizations_data, matched_detections)
  
  # Get the base stations that were excluded from each localization 
  # and evaluated features that are related to these base stations
  results <- calculate_missed_base_stations_features(localizations_data_with_features,
                                                     base_stations_info)
  
  localizations_data_with_features <- results$localizations_data
  missed_base_stations <- results$missed_base_stations
  
  return(list(
    localizations_data = localizations_data_with_features,
    participating_base_stations = participating_base_stations,
    missed_base_stations = missed_base_stations
  ))
  
}