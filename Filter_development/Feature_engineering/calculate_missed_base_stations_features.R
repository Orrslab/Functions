library(data.table)

calculate_missed_base_stations_features <- function(localizations_data, base_stations_info) {
  message("calculating features of missed base stations in each localization.")
  
  # Ensure the data.tables are sorted and properly typed
  loc_data <- as.data.table(localizations_data)
  bs_info <-  as.data.table(base_stations_info)
  
  # Convert loc_data$TIME from milliseconds to POSIXct
  loc_data[, TIME_sec := TIME / 1000]
  
  # Rename lat/lon columns in loc_data to avoid confusion with bs_lat/bs_lon
  setnames(loc_data, old = c("lat", "lon"), new = c("loc_lat", "loc_lon"))
  
  # Perform the non-equi join and keep only bs_lat and bs_lon from bs_info
  loc_with_bs <- loc_data[bs_info, 
                          on = .(TIME_sec >= Since, TIME_sec < Until), 
                          allow.cartesian = TRUE, nomatch = 0L]
  
  # FOR DEBUGGING PURPOSES
  # print(head(loc_with_bs[loc_with_bs$Radio_serial_number == 972006002, c("dateTime", "bs_lat", "bs_lon")], 100))
  # print(loc_with_bs[loc_with_bs$Radio_serial_number == 972006007, c("dateTime", "bs_lat", "bs_lon")])
  # print(head(loc_with_bs))
  
  # Compute the distance between each localization and each active base station
  loc_with_bs[, bs_distance := distHaversine(
    matrix(c(loc_lon, loc_lat), ncol = 2),
    matrix(c(bs_lon, bs_lat), ncol = 2)
  ), by = .(TAG, TIME)]
  
  # Filter missed base stations that are closer than the closest participating one
  missed_closer_bs <- loc_with_bs[bs_distance < Min_distance_to_BS] 
  # missed_closer_bs <- loc_with_bs[bs_distance < Max_distance_to_BS]  # Gives missed bs for almost all locations
  
  # Create a table of the missed base stations per localization
  missed_bs <- missed_closer_bs[, .(missed_bs_id = Radio_serial_number), by = .(TAG, TIME)]
  
  # Compute the missed base stations features
  missed_features <- missed_closer_bs[, .(
    num_missed_bs = .N,
    closest_missed_bs_distance = if (.N > 0) min(bs_distance) else NA_real_,
    mean_missed_bs_distance = if (.N > 1) mean(bs_distance) else NA_real_
  ), by = .(TAG, TIME)]
  
  # Merge features back to localizations_data
  localizations_data <- merge(localizations_data, missed_features, 
                              by = c("TAG", "TIME"), all.x = TRUE)
  
  # Replace NAs with 0 or NA_real_ as needed- in case num_missed_bs got NA value instead of zero
  localizations_data$num_missed_bs[is.na(localizations_data$num_missed_bs)] <- 0
  localizations_data$closest_missed_bs_distance[is.na(localizations_data$closest_missed_bs_distance) & 
                                                  localizations_data$num_missed_bs == 0] <- NA_real_
  localizations_data$mean_missed_bs_distance[is.na(localizations_data$mean_missed_bs_distance) & 
                                               localizations_data$num_missed_bs <= 1] <- NA_real_
  
  return(list(
    localizations_data = localizations_data,
    missed_base_stations = missed_bs
  ))

}