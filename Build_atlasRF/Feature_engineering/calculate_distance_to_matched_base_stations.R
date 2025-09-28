
source(file.path(getwd(), "Filter_development/Feature_engineering/get_bs_coordinates_from_matched_detections.R"))

#' Calculate Distance to Matched Base Stations
#'
#' Computes the geographic distance (in meters) between each localization point and its matched base station,
#' based on their respective coordinates.
#'
#' @param matched_detections A `data.frame` or `data.table` containing matched detections, including columns:
#' \describe{
#'   \item{TIME}{Timestamp of the detection (in milliseconds since epoch)}
#'   \item{roundTIME}{Rounded timestamp (in milliseconds since epoch)}
#'   \item{BS}{Base station ID}
#'   \item{lat, lon}{Latitude and longitude of the localization point}
#' }
#' @param base_stations_info A `data.frame` or `data.table` containing base station information, including coordinates.
#'
#' @return A `data.table` containing the input data with additional columns:
#' \describe{
#'   \item{loc_lat, loc_lon}{Renamed latitude and longitude of the localization}
#'   \item{bs_lat, bs_lon}{Latitude and longitude of the matched base station}
#'   \item{dist}{Distance in meters between the localization and the base station, calculated using the Haversine formula}
#' }
#'
#' @details
#' \itemize{
#'   \item Converts `TIME` from milliseconds to seconds.
#'   \item Converts `roundTIME` to `POSIXct` format.
#'   \item Joins base station coordinates to matched detections using \code{get_bs_coordinates_from_matched_detections}.
#'   \item Calculates distances with `geosphere::distHaversine`.
#' }
#'
#' @import data.table
#' @importFrom geosphere distHaversine
#'
#' @examples
#' \dontrun{
#' matched_with_dist <- calculate_distance_to_matched_base_stations(matched_detections, base_stations_info)
#' }
#'
#' @export

calculate_distance_to_matched_base_stations <- function (matched_detections, base_stations_info) {
  
  message("Calculating the distance to all the matched base stations.")
  
  # --- Prepare matched detections ---
  matched_with_dist <- as.data.table(matched_detections)
  
  # Convert TIME to seconds
  matched_with_dist[, TIME := TIME / 1000]
  
  # Make sure roundTIME is in POSIXct
  matched_with_dist[, roundTIME := as.POSIXct(roundTIME / 1000, origin = "1970-01-01", tz = "UTC")]
  
  # Make sure that BS is numeric
  matched_with_dist[, BS := as.numeric(BS)]
  
  # Rename localization coords for clarity- to distinguish them from the base stations' coordinates
  setnames(matched_with_dist, old = c("lat", "lon"), new = c("loc_lat", "loc_lon"))
  
  # For each detection, assign the coordinates of the corresponding base station
  matched_with_dist <- get_bs_coordinates_from_matched_detections(matched_with_dist, base_stations_info)
  
  # FOR DEBUGGING PURPOSES
  # print(head(loc_with_bs[loc_with_bs$Radio_serial_number == 972006002, c("dateTime", "BS", "bs_lat", "bs_lon")], 100))
  # print(matched_with_dist[matched_with_dist$BS == 972006004, c("dateTime", "BS", "bs_lat", "bs_lon")])
  
  # --- Check/convert all coordinate columns to numeric ---
  matched_with_dist[, c("bs_lat", "bs_lon", "loc_lat", "loc_lon") := lapply(.SD, as.numeric),
          .SDcols = c("bs_lat", "bs_lon", "loc_lat", "loc_lon")]
  
  # --- Compute distance from each localization to each base station ---
  matched_with_dist[, dist := mapply(function(lat1, lon1, lat2, lon2) {
    distHaversine(c(lon1, lat1), c(lon2, lat2))
  }, loc_lat, loc_lon, bs_lat, bs_lon)]
  
  return(matched_with_dist)
  
}
