source(file.path(getwd(), "match_detections_to_localizations.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_SNR_features.R"))
source(file.path(getwd(), "create_participating_base_stations_table.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_distance_to_closest_and_farthest_base_stations.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_distance_to_matched_base_stations.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_missed_base_stations_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_base_stations_convex_hull_polygon.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_circular_variance_of_participating_base_stations.R"))

#' Calculate Detection-Based Features for Each Localization
#'
#' Enriches each localization point with detection-based features derived from matched detections, including:
#' signal quality, spatial distribution of base stations, and identification of missed base stations.
#'
#' @param localization_data A `data.frame` or `data.table` containing localization data with columns such as `TAG`, `TIME`, `lat`, and `lon`.
#' @param detections_data A `data.frame` or `data.table` of raw ATLAS detections, including detection times and base station IDs.
#' @param base_stations_info A `data.frame` or `data.table` containing metadata about base stations, including location and identifiers.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{localization_data}{The input localization data augmented with detection-based features.}
#'   \item{participating_base_stations}{A `data.table` with the base stations that contributed to each localization.}
#'   \item{missed_base_stations}{A `data.table` or list of base stations that were expected to contribute but did not detect the beacon.}
#' }
#'
#' @details This function coordinates several processing steps:
#' \itemize{
#'   \item Matching detections to localization timestamps.
#'   \item Calculating signal-to-noise ratio (SNR) features.
#'   \item Computing distances to all matched base stations.
#'   \item Calculating convex hull geometry and circular variance of base stations.
#'   \item Identifying closest and farthest base stations.
#'   \item Detecting and analyzing missed base stations.
#' }
#' 
#' Helper functions must be sourced in advance:
#' \itemize{
#'   \item \code{\link{match_detections_to_localizations}}
#'   \item \code{\link{calculate_SNR_features}}
#'   \item \code{\link{calculate_distance_to_matched_base_stations}}
#'   \item \code{\link{calculate_distance_to_closest_and_farthest_base_stations}}
#'   \item \code{\link{calculate_base_stations_convex_hull_polygon}}
#'   \item \code{\link{calculate_circular_variance_of_participating_base_stations}}
#'   \item \code{\link{create_participating_base_stations_table}}
#'   \item \code{\link{calculate_missed_base_stations_features}}
#' }
#'
#' @examples
#' \dontrun{
#' result <- calculate_detection_based_features(localization_data, detections_data, base_stations_info)
#' enriched_data <- result$localization_data
#' bs_participation <- result$participating_base_stations
#' missed_bs <- result$missed_base_stations
#' }
#'
#' @import data.table
#' @export

# Features per location, that reqire knowing the detections that correspond to each location
calculate_detection_based_features <- function(localization_data, 
                                               detections_data,
                                               base_stations_info) {
  
  # Get the detections corresponding to each location
  matched_detections <- match_detections_to_localizations(localization_data, detections_data)
  
  # Calculate the SNR features
  localization_data <- calculate_SNR_features(matched_detections, localization_data)
  
  # Calculate the distance to all matched base stations
  matched_with_dist <- calculate_distance_to_matched_base_stations(matched_detections, base_stations_info)
  
  # Create a table with the participating base stations in each localization
  participating_base_stations <- create_participating_base_stations_table(localization_data, matched_with_dist)
  
  # # Calculate the distribution of the base stations, relative to the convex hull polygon --- #
  # localization_data <- calculate_base_stations_convex_hull_polygon(matched_with_dist, localization_data)
  
  # Calculate the circular variance of the participating base stations
  localization_data <- calculate_circular_variance_of_participating_base_stations(localization_data, matched_with_dist)
  
  # Calculate the distance of each location from the closest base station
  localization_data <- calculate_distance_to_closest_and_farthest_base_stations(
    localization_data,
    matched_with_dist)
  
  # Get the base stations that were excluded from each localization 
  # and evaluated features that are related to these base stations
  results <- calculate_missed_base_stations_features(localization_data,
                                                     base_stations_info)
  
  localization_data <- results$localization_data
  missed_base_stations <- results$missed_base_stations
  
  return(list(
    localization_data = localization_data,
    participating_base_stations = participating_base_stations,
    missed_base_stations = missed_base_stations
  ))
  
}