library(data.table)

merge_closest_bs_with_beacons_info <- function(closest_bs, beacons_detection_ratio_per_hour) {
  
  # Ensure inputs are data.tables
  closest_bs <- as.data.table(closest_bs)
  beacons_detection_ratio_per_hour <- as.data.table(beacons_detection_ratio_per_hour)
  
  # Convert BS and HOUR to character in both tables to ensure compatibility
  closest_bs[, `:=`(BS = as.character(BS), HOUR = as.character(HOUR))]
  beacons_detection_ratio_per_hour[, `:=`(BS = as.character(BS), HOUR = as.character(HOUR))]
  
  # Merge by BS and HOUR to bring in BEACONS_DETECTION_MULTIPLICATION
  merged <- merge(
    closest_bs,
    beacons_detection_ratio_per_hour,
    by = c("BS", "HOUR"),
    all.x = TRUE
  )
  
  return(merged)
}