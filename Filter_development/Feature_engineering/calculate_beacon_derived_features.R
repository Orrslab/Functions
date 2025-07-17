
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_beacons_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/count_low_beacon_detections_per_base_station_per_hour.R"))

calculate_beacon_derived_features <- function(localization_data, 
                                              participating_base_stations, 
                                              beacons_detection_ratio_per_hour,
                                              base_stations_summary_per_beacon,
                                              low_beacon_detection_fraction) {
  
  # --- Calculate features for the closest three base stations
  
  beacon_features_3_closest_bs <- calculate_beacons_features(participating_base_stations,
                                                             number_of_closest_bs = 3,
                                                             beacons_detection_ratio_per_hour)
  
  # Change the detection_probability_factor column's name
  setnames(beacon_features_3_closest_bs, "detection_probability_factor", "detection_probability_factor_3_closest_bs")
  setnames(beacon_features_3_closest_bs, "mean_max_snr_mean_ratio", "mean_max_snr_mean_ratio_3_closest_bs")
  setnames(beacon_features_3_closest_bs, "num_low_detection_beacons_by_participating_bs", "num_low_detection_beacons_by_participating_bs_3_closest_bs")
  
  # Merge beacon features into localization_data by TAG and TIME
  localization_data <- merge(
    localization_data,
    beacon_features_3_closest_bs,
    by = c("TAG", "TIME"),
    all.x = TRUE  # Keep all rows from localization_data
  )
  
  # --- Calculate features for the closest five base stations
  
  beacon_features_5_closest_bs <- calculate_beacons_features(participating_base_stations,
                                                             number_of_closest_bs = 5,
                                                             beacons_detection_ratio_per_hour)
  
  # Change the detection_probability_factor column's name
  setnames(beacon_features_5_closest_bs, "detection_probability_factor", "detection_probability_factor_5_closest_bs")
  setnames(beacon_features_5_closest_bs, "mean_max_snr_mean_ratio", "mean_max_snr_mean_ratio_5_closest_bs")
  setnames(beacon_features_5_closest_bs, "num_low_detection_beacons_by_participating_bs", "num_low_detection_beacons_by_participating_bs_5_closest_bs")
  
  # Merge beacon features into localization_data by TAG and TIME
  localization_data <- merge(
    localization_data,
    beacon_features_5_closest_bs,
    by = c("TAG", "TIME"),
    all.x = TRUE  # Keep all rows from localization_data
  )
  
  #######
  
  # Binary feature: no beacons were detected by any of the participating base stations (binary)
  
  participating_base_stations <- as.data.table(participating_base_stations)
  base_stations_summary_per_beacon <- as.data.table(base_stations_summary_per_beacon)
  
  # Create a table with base station, hour, and counts of low detections per base station per hour
  low_beacons_per_bs_per_hour_dt <- count_low_beacon_detections_per_base_station_per_hour(
    base_stations_summary_per_beacon, low_beacon_detection_fraction)
  
  # Add HOUR to participating_base_stations
  participating_base_stations[, HOUR := as.numeric(floor_date(as.POSIXct(TIME / 1000, origin = "1970-01-01", tz = "UTC"), unit = "hour")) * 1000]
  
  # Format BS as character
  participating_base_stations[, BS := as.character(participating_bs_id)]
  
  # Change format
  low_beacons_per_bs_per_hour_dt[, BS := as.character(BS)]
  
  # Join by BS and HOUR, then group by (TAG, TIME) and sum LOW_DETECTIONS
  bs_with_low_detections_per_hour <- merge(
    participating_base_stations[, .(TAG, TIME, BS, HOUR)],
    low_beacons_per_bs_per_hour_dt,
    by = c("BS", "HOUR"),
    all.x = TRUE
  )
  
  # Replace NA in ALL_LOW_BEACONS with FALSE (meaning not all beacons were low)
  bs_with_low_detections_per_hour[is.na(ALL_LOW_BEACONS), ALL_LOW_BEACONS := 0]
  
  # Group by TAG and TIME to calculate:
  # 1. Number of BS where all beacons had low detections
  # 2. Total number of participating BS
  # 3. Fraction of such BS
  no_beacons_detected_by_group <- bs_with_low_detections_per_hour[, .(
    num_bs_with_all_low_beacons = sum(ALL_LOW_BEACONS, na.rm = TRUE),
    num_participating_bs = .N
  ), by = .(TAG, TIME)]
  
  no_beacons_detected_by_group[, frac_bs_with_all_low_beacons := 
                                 num_bs_with_all_low_beacons / num_participating_bs]
  
  # Merge into localization_data
  localization_data <- merge(
    localization_data,
    no_beacons_detected_by_group[, .(TAG, TIME, num_bs_with_all_low_beacons, frac_bs_with_all_low_beacons)],
    by = c("TAG", "TIME"),
    all.x = TRUE
  )
  
  #### Results Analysis
  # Count by num_bs_with_all_low_beacons and Outliers
  count_by_num_bs <- localization_data[
    , .N, by = .(num_bs_with_all_low_beacons, Outliers)
  ][order(num_bs_with_all_low_beacons, Outliers)]

  # Print the result
  cat("Counts by num_bs_with_all_low_beacons and Outliers:\n")
  print(count_by_num_bs)

  # Count by frac_bs_with_all_low_beacons and Outliers
  count_by_frac_bs <- localization_data[
    , .N, by = .(frac_bs_with_all_low_beacons, Outliers)
  ][order(frac_bs_with_all_low_beacons, Outliers)]

  # Print the result
  cat("\nCounts by frac_bs_with_all_low_beacons and Outliers:\n")
  print(count_by_frac_bs)
  ####
  
  return(localization_data)
  
}