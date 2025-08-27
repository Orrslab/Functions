# The function assumes that the info tables are formatted

library(dplyr)
library(lubridate)
library(geosphere)
library(tidyr)

generate_beacons_base_station_summary_table_over_time_periods <- function(beacons_info, bs_info, infinity_time, bs_summary_data, min_detections_number = 1000) {
  
  # Ensure consistent locale for parsing- without it the code does not convert September dates correctly...
  Sys.setlocale("LC_TIME", "C")
  
  # Prepare clean info tables of the base stations and beacons data
  # Beacons info
  beacons_clean <- beacons_info %>%
    rename(
      TAG = TAG,
      TAG_SINCE = Since,
      TAG_UNTIL = Until,
      LAT_tag = Latitude,
      LON_tag = Longitude
    ) %>%
    mutate(
      TAG_UNTIL = ifelse(as.character(TAG_UNTIL) == "Infinity", infinity_time, as.character(TAG_UNTIL)),
      TAG_SINCE = as.POSIXct(as.character(TAG_SINCE), format = "%Y-%b-%d %H:%M", tz = "UTC"),
      TAG_UNTIL = as.POSIXct(TAG_UNTIL, format = "%Y-%b-%d %H:%M", tz = "UTC")
    )
  
  # Base stations info
  bs_clean <- bs_info %>%
    rename(
      BS = Radio_serial_number,
      BS_SINCE = Since,
      BS_UNTIL = Until,
      LAT_bs = Latitude,
      LON_bs = Longitude
    ) %>%
    mutate(
      BS_UNTIL = ifelse(as.character(BS_UNTIL) == "Infinity", infinity_time, as.character(BS_UNTIL)),
      BS_SINCE = as.POSIXct(as.character(BS_SINCE), format = "%Y-%b-%d %H:%M", tz = "UTC"),
      BS_UNTIL = as.POSIXct(BS_UNTIL, format = "%Y-%b-%d %H:%M", tz = "UTC")
    )
  
  # Drop duplicate column names in the beacons and base stations info tables
  beacons_clean <- beacons_clean %>% select(-Name, -Elevation)
  bs_clean <- bs_clean %>% select(-Name, -Elevation)
  
  # Create all TAG–BS pairs and calculate overlap period
  overlap_table <- tidyr::crossing(beacons_clean, bs_clean) %>%
    mutate(
      OVERLAP_START = pmax(TAG_SINCE, BS_SINCE),
      OVERLAP_END = pmin(TAG_UNTIL, BS_UNTIL),
      VALID = OVERLAP_START < OVERLAP_END
    ) %>%
    filter(VALID) %>%
    mutate(OVERLAP_HOURS = as.numeric(difftime(OVERLAP_END, OVERLAP_START, units = "hours"))) %>%
    rowwise() %>%
    mutate(DISTANCE_M = distHaversine(c(LON_tag, LAT_tag), c(LON_bs, LAT_bs))) %>%
    ungroup() %>%
    select(TAG, BS, OVERLAP_START, OVERLAP_END, OVERLAP_HOURS, DISTANCE_M)
  
  # # For debug purposes
  # overlap_table %>%
  #   filter(TAG == 972006000007, BS == 972006006) %>%
  #   print(n = Inf)
  
  # Prepare hourly detection data
  bs_summary_data <- bs_summary_data %>%
    mutate(HOUR_POSIX = as.POSIXct(HOUR / 1000, origin = "1970-01-01", tz = "UTC"))
  
  # Convert the base station number to character in the overlap table
  overlap_table <- overlap_table %>%
    mutate(BS = as.character(BS))
  
  # Convert the base station number to character in the base stations detections data
  bs_summary_data <- bs_summary_data %>%
    mutate(BS = as.character(BS))
  
  # Summarize detection statistics
  beacons_detection_ratio_per_bs <- overlap_table %>%
    rowwise() %>%
    mutate(
      TOTAL_DETECTIONS = sum(
        bs_summary_data$DETECTIONS[
          bs_summary_data$TAG == TAG &
            bs_summary_data$BS == BS &
            bs_summary_data$HOUR_POSIX >= OVERLAP_START &
            bs_summary_data$HOUR_POSIX < OVERLAP_END
        ],
        na.rm = TRUE
      ),
      NORMALIZED_DETECTIONS = ifelse(OVERLAP_HOURS == 0, NA, TOTAL_DETECTIONS / (OVERLAP_HOURS * 3600)),
      HOURS_WITH_DETECTIONS = sum(
        bs_summary_data$TAG == TAG &
          bs_summary_data$BS == BS &
          bs_summary_data$HOUR_POSIX >= OVERLAP_START &
          bs_summary_data$HOUR_POSIX < OVERLAP_END &
          bs_summary_data$DETECTIONS >= min_detections_number,
        na.rm = TRUE
      ),
      DETECTION_RATIO = ifelse(OVERLAP_HOURS == 0, NA, HOURS_WITH_DETECTIONS / OVERLAP_HOURS)
    ) %>%
    ungroup()
  
  return(beacons_detection_ratio_per_bs)
}