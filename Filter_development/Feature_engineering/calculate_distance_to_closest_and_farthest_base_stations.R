library(data.table)
library(geosphere)

source(file.path(getwd(), "Filter_development/Feature_engineering/get_bs_coordinates_from_matched_detections.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_base_stations_convex_hull_polygon.R"))

#' Calculate distances to closest and farthest base stations
#'
#' For each localization point, this function computes the distances to all detected base stations
#' and identifies the closest and farthest base stations using the Haversine formula.
#'
#' @param localizations_data A `data.frame` or `data.table` containing localization records with at least `TAG`, `TIME`, `lat`, and `lon` columns.
#' @param matched_detections A `data.frame` or `data.table` of detection records that were matched to localizations, including `TIME`, `BS`, `SNR`, and location coordinates.
#' @param base_stations_info A `data.table` containing base station metadata with coordinates and activation times.
#'
#' @return A `data.frame` of the original localization data with four new columns:
#' \describe{
#'   \item{Min_distance_to_BS}{Distance in meters to the closest base station}
#'   \item{Closest_BS}{Base station ID of the closest base station}
#'   \item{Max_distance_to_BS}{Distance in meters to the farthest base station}
#'   \item{Farthest_BS}{Base station ID of the farthest base station}
#' }
#'
#' @details
#' \itemize{
#'   \item Converts times to Unix seconds and aligns timestamps between localizations and detections
#'   \item Retrieves base station coordinates for each detection using \code{\link{get_bs_coordinates_from_matched_detections}}
#'   \item Computes geodesic distances using the Haversine formula via \code{\link[geosphere]{distHaversine}}
#'   \item Merges distance information back into the original localization data
#' }
#'
#' @import data.table
#' @import geosphere
#' @seealso \code{\link{get_bs_coordinates_from_matched_detections}}
#'
#' @examples
#' \dontrun{
#' result <- calculate_distance_to_closest_and_farthest_base_stations(localizations_data, matched_detections, base_stations_info)
#' }
#'
#' @export

calculate_distance_to_closest_and_farthest_base_stations <- function(localizations_data, matched_detections, base_stations_info) {
  message("Calculating the distance to the closest base station.")
  
  # --- Prepare matched detections ---
  matched <- as.data.table(matched_detections)
  
  # Convert TIME to seconds
  matched[, TIME := TIME / 1000]
  
  # Make sure roundTIME is in POSIXct
  matched[, roundTIME := as.POSIXct(roundTIME / 1000, origin = "1970-01-01", tz = "UTC")]
  
  # Make sure that BS is numeric
  matched[, BS := as.numeric(BS)]
  
  # Rename localization coords for clarity- to distinguish them from the base stations' coordinates
  setnames(matched, old = c("lat", "lon"), new = c("loc_lat", "loc_lon"))
  
  # For each detection, assign the coordinates of the corresponding base station
  matched <- get_bs_coordinates_from_matched_detections(matched, base_stations_info)
  
  # FOR DEBUGGING PURPOSES
  # print(head(loc_with_bs[loc_with_bs$Radio_serial_number == 972006002, c("dateTime", "BS", "bs_lat", "bs_lon")], 100))
  # print(matched[matched$BS == 972006004, c("dateTime", "BS", "bs_lat", "bs_lon")])
  
  # --- Check/convert all coordinate columns to numeric ---
  matched[, c("bs_lat", "bs_lon", "loc_lat", "loc_lon") := lapply(.SD, as.numeric),
          .SDcols = c("bs_lat", "bs_lon", "loc_lat", "loc_lon")]
  
  # --- Calculate the distribution of the base stations --- #
  localizations_data <- calculate_base_stations_convex_hull_polygon(matched, localizations_data)

  # --- Compute distance from each localization to each base station ---
  matched[, dist := mapply(function(lat1, lon1, lat2, lon2) {
    distHaversine(c(lon1, lat1), c(lon2, lat2))
  }, loc_lat, loc_lon, bs_lat, bs_lon)]

  # --- Find the closest base station per point ---
  closest <- matched[, .SD[which.min(dist)], by = .(TAG, roundTIME)]
  closest <- closest[, .(TAG, roundTIME, Min_distance_to_BS = dist, Closest_BS = BS)]

  # --- Find the farthest base station per point ---
  farthest <- matched[, .SD[which.max(dist)], by = .(TAG, roundTIME)]
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