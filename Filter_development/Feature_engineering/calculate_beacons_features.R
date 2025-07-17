
library(lubridate)
library(data.table)

source(file.path(getwd(), "Filter_development/Feature_engineering/get_closest_base_stations.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/merge_closest_bs_with_beacons_info.R"))


calculate_beacons_features <- function(participating_base_stations,
                                       number_of_closest_bs,
                                       beacons_detection_ratio_per_hour) {
  
  # Get the three closest base stations to each localization
  closest_bs <- get_closest_base_stations(participating_base_stations, number_of_closest_bs)
  
  # Get the floor hour of each localization's closest base station
  closest_bs[, HOUR := as.numeric(floor_date(as.POSIXct(TIME / 1000, origin = "1970-01-01", tz = "UTC"), unit = "hour")) * 1000]
  
  # Calculate the multiplication of the beacons' detections multiplication
  merged_closest_bs_with_beacons_info <- merge_closest_bs_with_beacons_info(closest_bs, beacons_detection_ratio_per_hour)
  
  # --- Feature 1: detection probability factor
  detection_probability_factor_df <- merged_closest_bs_with_beacons_info[, .(
    detection_probability_factor = prod(BEACONS_DETECTION_MULTIPLICATION, na.rm = TRUE)
  ), by = .(TAG, TIME)]
  
  # --- Feature 2: mean MAX_SNR_MEAN_RATIO per localization
  mean_snr_per_localization <- merged_closest_bs_with_beacons_info[, .(
    mean_max_snr_mean_ratio = mean(MAX_SNR_MEAN_RATIO, na.rm = TRUE)
  ), by = .(TAG, TIME)]
  
  # --- Feature 3: number of low detection beacons from participating BS
  
  # Convert to data table
  closest_bs <- as.data.table(closest_bs)
  beacons_detection_ratio_per_hour <- as.data.table(beacons_detection_ratio_per_hour)
  
  # Ensure matching types
  closest_bs[, BS := as.character(BS)]
  beacons_detection_ratio_per_hour[, BS := as.character(BS)]
  
  low_detection_info <- merge(
    closest_bs[, .(TAG, TIME, BS, HOUR)],
    beacons_detection_ratio_per_hour[, .(BS, HOUR, NUM_LOW_DETECTION_BEACONS)],
    by = c("BS", "HOUR"),
    all.x = TRUE
  )
  
  num_low_detection_beacons <- low_detection_info[, .(
    num_low_detection_beacons_by_participating_bs = sum(NUM_LOW_DETECTION_BEACONS, na.rm = TRUE)
  ), by = .(TAG, TIME)]
  
  # --- Merge all features into one data.table
  beacon_features_df <- Reduce(function(x, y) merge(x, y, by = c("TAG", "TIME"), all = TRUE),
                               list(
                                 detection_probability_factor_df,
                                 mean_snr_per_localization,
                                 num_low_detection_beacons
                               )
  )
  
  return(beacon_features_df)
  
}