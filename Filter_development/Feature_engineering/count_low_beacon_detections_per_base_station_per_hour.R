
count_low_beacon_detections_per_base_station_per_hour <- function(base_stations_summary_per_beacon,
                                                                  low_beacon_detection_fraction = 0.6) {
  
  # Evaluate LOW_DETECTIONS per beacon per base station per hour
  base_stations_summary_per_beacon[, LOW_DETECTIONS := as.integer(DETECTIONS < 3600 * low_beacon_detection_fraction)]
  
  # Count total detected beacons and low-detection, per base station per hour
  summary_dt <- base_stations_summary_per_beacon[, .(
    NUM_DETECTED_BEACONS = .N,  
    NUM_LOW_BEACONS = sum(LOW_DETECTIONS, na.rm = TRUE)
  ), by = .(BS, HOUR)]
  
  summary_dt[, ALL_LOW_BEACONS := as.integer(NUM_LOW_BEACONS == NUM_DETECTED_BEACONS)]
  
  return(summary_dt)
  
}