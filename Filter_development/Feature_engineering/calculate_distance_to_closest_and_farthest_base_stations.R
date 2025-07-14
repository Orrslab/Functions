library(data.table)
library(geosphere)

#' Calculate Distances to Closest and Farthest Base Stations
#'
#' Computes the distances from each localization point to all matched base stations and identifies
#' the closest and farthest base stations based on geodesic distance.
#'
#' @param localizations_data A `data.frame` or `data.table` with localization data, including:
#' \describe{
#'   \item{TAG}{Animal's tag ID}
#'   \item{TIME}{Timestamp of localization (in milliseconds since epoch)}
#'   \item{lat, lon}{Latitude and longitude of the localization point}
#' }
#' @param matched_detections_with_dist A `data.frame` or `data.table` of detection records that include:
#' \describe{
#'   \item{TAG, roundTIME, BS}{Tag ID, rounded time, and base station ID}
#'   \item{dist}{Distance from the localization point to the base station (in meters)}
#' }
#' @param base_stations_info A `data.table` containing metadata on base stations (not used directly in this function, but required in upstream calculations).
#'
#' @return A `data.frame` with the original localization data plus four new columns:
#' \describe{
#'   \item{Min_distance_to_BS}{Minimum distance (m) to any participating base station}
#'   \item{Closest_BS}{Base station ID with the minimum distance}
#'   \item{Max_distance_to_BS}{Maximum distance (m) to any participating base station}
#'   \item{Farthest_BS}{Base station ID with the maximum distance}
#' }
#'
#' @details
#' \itemize{
#'   \item Uses `dist` column in `matched_detections_with_dist`, assumed to be precomputed with \code{geosphere::distHaversine}.
#'   \item `roundTIME` in `localizations_data` is computed from `TIME` if not already present, rounding to nearest second.
#'   \item Finds closest and farthest base stations per `(TAG, roundTIME)` group and merges the result into the localization data.
#'   \item Removes `roundTIME` after merging.
#' }
#'
#' @import data.table
#' @seealso \code{\link{calculate_distance_to_matched_base_stations}}
#'
#' @examples
#' \dontrun{
#' distances_df <- calculate_distance_to_closest_and_farthest_base_stations(localizations_data, matched_detections_with_dist, base_stations_info)
#' }
#'
#' @export

calculate_distance_to_closest_and_farthest_base_stations <- function(localizations_data, matched_detections_with_dist) {

  # --- Find the closest base station per point ---
  closest <- matched_detections_with_dist[, .SD[which.min(dist)], by = .(TAG, roundTIME)]
  closest <- closest[, .(TAG, roundTIME, Min_distance_to_BS = dist, Closest_BS = BS)]

  # --- Find the farthest base station per point ---
  farthest <- matched_detections_with_dist[, .SD[which.max(dist)], by = .(TAG, roundTIME)]
  farthest <- farthest[, .(TAG, roundTIME, Max_distance_to_BS = dist, Farthest_BS = BS)]

  # --- Merge back into full localization dataset ---
  localizations_data <- as.data.table(localizations_data)
  localizations_data[, roundTIME := as.POSIXct(round(TIME / 1000), origin = "1970-01-01", tz = "UTC")]
  
  result <- merge(localizations_data, closest, by = c("TAG", "roundTIME"), all.x = TRUE)
  result <- merge(result, farthest, by = c("TAG", "roundTIME"), all.x = TRUE)
  
  # Remove the roundTIME column from the Localizations table
  result[, roundTIME := NULL]
  
  return(as.data.frame(result))
}