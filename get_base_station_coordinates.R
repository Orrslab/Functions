
get_base_station_coordinates <- function(base_station_number, location_time_unix, base_stations_info) {
  # # Parse time columns in bs_info (only once is enough if already parsed externally)
  # base_stations_info[, Since := as.numeric(as.POSIXct(Since, format = "%Y-%b-%d %H:%M", tz = "UTC"))]
  # base_stations_info[, Until := ifelse(Until == "Infinity", Inf,
  #                           as.numeric(as.POSIXct(Until, format = "%Y-%b-%d %H:%M", tz = "UTC")))]
  
  # Filter for the given base station and time
  bs_subset <- base_stations_info[Radio_serial_number == base_station_number &
                         Since <= location_time_unix & Until > location_time_unix]
  
  if (nrow(bs_subset) == 0) return(NULL)
  
  return(list(lon = bs_subset$Longitude[1], lat = bs_subset$Latitude[1]))
}