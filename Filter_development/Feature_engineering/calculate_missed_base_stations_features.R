library(data.table)

calculate_missed_base_stations_features <- function(localizations_data, base_stations_info) {
  
  # Ensure the data.tables are sorted and properly typed
  loc_data <- as.data.table(localizations_data)
  bs_info <-  as.data.table(base_stations_info)
  
  # Convert loc_data$TIME from milliseconds to POSIXct
  loc_data[, TIME_sec := TIME / 1000]
  
  
  # Perform the non-equi join and keep only bs_lat and bs_lon from bs_info
  loc_with_bs <- loc_data[bs_info, 
                          on = .(TIME_sec >= Since, TIME_sec < Until), 
                          allow.cartesian = TRUE, nomatch = 0L]
  
  # FOR DEBUGGING PURPOSES
  # print(head(loc_with_bs[loc_with_bs$Radio_serial_number == 972006002, c("dateTime", "bs_lat", "bs_lon")], 100))
  # print(loc_with_bs[loc_with_bs$Radio_serial_number == 972006007, c("dateTime", "bs_lat", "bs_lon")])
  
  return(localizations_data)

}