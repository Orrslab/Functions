library(dplyr)
library(tidyr)

generate_bs_hour_detection_rate_table_from_beacons <- function(bs_summary_data, detection_summary, min_detection_ratio = 0.5, max_distance_beacon_bs_km = 7, low_detection_fraction = 0.6) {
  
  # Filter the detection_summary table by the minimum detection ratio
  expected_detections <- detection_summary %>%
    filter(DETECTION_RATIO > min_detection_ratio)
  
  # # Filter the detection_summary table by the maximum distance between the beacon and each base station
  # expected_detections <- detection_summary %>%
  #   filter(DISTANCE_M < max_distance_beacon_bs_km*1000)
  
  # Convert the base station number to character to match formats between the data frames
  bs_summary_data <- bs_summary_data %>%
    mutate(BS = as.character(BS))
  
  detection_summary <- detection_summary %>%
    mutate(BS = as.character(BS))
  
  # Create a table of expected beacons for each BS and hour
  bs_hour_detection <- bs_summary_data %>%
    mutate(
      HOUR_POSIX = as.POSIXct(HOUR / 1000, origin = "1970-01-01", tz = "UTC")
      ) %>%
    dplyr::select(BS, TAG, HOUR, HOUR_POSIX, DETECTIONS, MAX_SNR) %>%
    left_join(
      expected_detections %>%
        dplyr::select(TAG, BS, OVERLAP_START, OVERLAP_END),
      by = c("TAG", "BS"), 
      relationship = "many-to-many"
    ) %>%
    filter(HOUR_POSIX >= OVERLAP_START, HOUR_POSIX < OVERLAP_END) %>%
    mutate(NORMALIZED_DETECTIONS = DETECTIONS / 3600)
  
  # print(bs_hour_detection)
  
  # Calculate the product of normalized detections per BS and HOUR
  bs_hour_summary <- bs_hour_detection %>%
    group_by(BS, HOUR) %>%
    summarise(
      TOTAL_DETECTIONS = sum(DETECTIONS, na.rm = TRUE),
      TOTAL_MAX_SNR = sum(MAX_SNR, na.rm = TRUE),
      NUM_EXPECTED_BEACONS = n_distinct(TAG),
      NUM_LOW_DETECTION_BEACONS = sum(NORMALIZED_DETECTIONS < low_detection_fraction, na.rm = TRUE),
      BEACONS_DETECTION_MULTIPLICATION = prod(NORMALIZED_DETECTIONS, na.rm = TRUE),
      TAGS_EXPECTED = list(unique(TAG)),
      .groups = "drop"
    ) %>%
    mutate(
      BEACONS_DETECTION_RATE = TOTAL_DETECTIONS / (NUM_EXPECTED_BEACONS * 3600),
      MAX_SNR_MEAN_RATIO = TOTAL_MAX_SNR / NUM_EXPECTED_BEACONS
    ) %>%
    dplyr::select(BS, HOUR, NUM_EXPECTED_BEACONS, NUM_LOW_DETECTION_BEACONS, BEACONS_DETECTION_MULTIPLICATION, BEACONS_DETECTION_RATE, MAX_SNR_MEAN_RATIO, TAGS_EXPECTED)
  
  
  # # Group by BS and HOUR, then compute the rate
  # bs_hour_summary <- bs_hour_detection %>%
  #   group_by(BS, HOUR) %>%
  #   summarise(
  #     NUM_EXPECTED_BEACONS = n_distinct(TAG),
  #     TOTAL_DETECTIONS = sum(DETECTIONS, na.rm = TRUE),
  #     TOTAL_MAX_SNR = sum(MAX_SNR, na.rm = TRUE),
  #     TAGS_EXPECTED = list(unique(TAG)),
  #     .groups = "drop"
  #   ) %>%
  #   mutate(
  #     BEACONS_DETECTION_RATE = TOTAL_DETECTIONS / (NUM_EXPECTED_BEACONS * 3600),
  #     MAX_SNR_TOTAL_RATIO = TOTAL_MAX_SNR / NUM_EXPECTED_BEACONS
  #   ) %>%
  #   select(BS, HOUR, BEACONS_DETECTION_RATE, MAX_SNR_TOTAL_RATIO, TAGS_EXPECTED)
  
  return(bs_hour_summary)
}