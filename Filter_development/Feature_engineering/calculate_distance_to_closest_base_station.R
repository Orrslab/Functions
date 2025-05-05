library(data.table)
library(geosphere)

calculate_distance_to_closest_base_station <- function(matched_detections, localizations_data) {
  message("Calculating the distance to the closest base station.")
  
  # --- Load and preprocess base station info ---
  base_stations_info_path <- "C:/Users/netat/Documents/Movement_Ecology/ATLAS/Base_stations_beacons_info/Base_stations_info.csv"
  bs_info <- fread(base_stations_info_path)
  setnames(bs_info, "Radio_serial_number", "BS")
  
  bs_info[, Until := ifelse(Until == "Infinity", "2100-Jan-01 00:00", Until)]
  
  # Ensure consistent locale for parsing- without it the code does not convert September dates correctly...
  Sys.setlocale("LC_TIME", "C")
  
  # Convert times to POSIXct (correct time format assumed)
  bs_info[, start := as.POSIXct(Since, format = "%Y-%b-%d %H:%M", tz = "UTC")]
  bs_info[, end := as.POSIXct(Until, format = "%Y-%b-%d %H:%M", tz = "UTC")]
  
  # Filter relevant columns and rename to distinguish them from the localizations' coordinates
  bs_info <- bs_info[, .(BS, bs_lat = Latitude, bs_lon = Longitude, start, end)]
  
  # --- Prepare matched detections ---
  matched <- as.data.table(matched_detections)
  
  # Make sure TIME is in POSIXct
  matched[, roundTIME := as.POSIXct(TIME / 1000, origin = "1970-01-01", tz = "UTC")]
  
  # time point = interval with same start/end- to later get the corresponding base station coordinates in this time
  matched[, `:=`(start = roundTIME, end = roundTIME)]  
  
  # Set keys for foverlaps (must be on POSIXct!)
  setkey(bs_info, BS, start, end)
  setkey(matched, BS, start, end)
  
  # --- Join base station info to matched detections, based on time overlap ---
  matched <- foverlaps(matched, bs_info, type = "within", nomatch = 0)
  
  # Rename localization coords for clarity- to distinguish them from the base stations' coordinates
  setnames(matched, old = c("lat", "lon"), new = c("loc_lat", "loc_lon"))
  
  # --- Check/convert all coordinate columns to numeric ---
  matched[, c("bs_lat", "bs_lon", "loc_lat", "loc_lon") := lapply(.SD, as.numeric),
          .SDcols = c("bs_lat", "bs_lon", "loc_lat", "loc_lon")]
  
  # --- Compute distance from each localization to each base station ---
  matched[, dist := mapply(function(lat1, lon1, lat2, lon2) {
    distHaversine(c(lon1, lat1), c(lon2, lat2))
  }, loc_lat, loc_lon, bs_lat, bs_lon)]
  
  # --- Find the closest base station per point ---
  closest <- matched[, .SD[which.min(dist)], by = .(TAG, TIME)]
  closest <- closest[, .(TAG, TIME, Min_distance_to_BS = dist, Closest_BS = BS)]
  
  # --- Merge back into full localization dataset ---
  result <- merge(localizations_data, closest, by = c("TAG", "TIME"), all.x = TRUE)
  
  return(result)
}