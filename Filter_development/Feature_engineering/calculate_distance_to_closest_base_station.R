library(data.table)
library(geosphere)

# source(file.path(getwd(), "get_base_station_coordinates.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/get_bs_coordinates_from_matched_detections.R"))

calculate_distance_to_closest_base_station <- function(matched_detections, localizations_data) {
  message("Calculating the distance to the closest base station.")
  
  # --- Load and preprocess base station info ---
  base_stations_info_path <- "C:/Users/netat/Documents/Movement_Ecology/ATLAS/Base_stations_beacons_info/Base_stations_info.csv"
  bs_info <- fread(base_stations_info_path)
  
  bs_info[, Until := ifelse(Until == "Infinity", "2100-Jan-01 00:00", Until)]
  
  # Ensure consistent locale for parsing- without it the code does not convert September dates correctly...
  Sys.setlocale("LC_TIME", "C")
  
  # Convert bs times to POSIXct (correct time format assumed)
  bs_info[, Since := as.POSIXct(Since, format = "%Y-%b-%d %H:%M", tz = "UTC")]
  bs_info[, Until := as.POSIXct(Until, format = "%Y-%b-%d %H:%M", tz = "UTC")]
  
  # Convert bs times to numeric (Unix timestamp)
  bs_info[, Since := as.numeric(Since)]
  bs_info[, Until := as.numeric(Until)]
  
  # Filter relevant columns and rename to distinguish them from the localizations' coordinates
  bs_info <- bs_info[, .(Radio_serial_number, bs_lat = Latitude, bs_lon = Longitude, Since, Until)]
  
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
  matched <- get_bs_coordinates_from_matched_detections(matched, bs_info)

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

  # --- Merge back into full localization dataset ---
  localizations_data <- as.data.table(localizations_data)
  localizations_data[, roundTIME := as.POSIXct(round(TIME / 1000), origin = "1970-01-01", tz = "UTC")]
  
  result <- merge(localizations_data, closest, by = c("TAG", "roundTIME"), all.x = TRUE)
  result[, roundTIME := NULL]
  
  return(as.data.frame(result))
}