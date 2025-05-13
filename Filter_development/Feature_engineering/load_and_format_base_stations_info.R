
load_and_format_base_stations_info <- function(base_stations_info_path) {
  
  # Load the base stations info
  bs_info <- fread(base_stations_info_path)
  
  # Change "Infinity in the Since column to a far future data
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
  
  return(bs_info)
}

