library(data.table)
library(geosphere)

# source(file.path(getwd(), "get_base_station_coordinates.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/get_bs_coordinates_from_matched_detections.R"))

calculate_distance_to_closest_base_station <- function(localizations_data, matched_detections, base_stations_info) {
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