library(dplyr)
library(tidyr)

#' Generate hourly base station detection rate summary from beacon data
#'
#' This function computes per-hour detection statistics for each base station (BS) 
#' using beacon-derived detection data. It aggregates detections across beacons 
#' that are expected to be detected by a BS within a given time window, and 
#' calculates summary metrics such as normalized detection rates, beacon 
#' detection multiplication, and average SNR ratios. 
#'
#' @param bs_summary_data A data frame containing hourly detection summaries 
#' per base station and beacon. Must include columns: 
#' \code{BS}, \code{TAG}, \code{HOUR} (milliseconds since epoch), 
#' \code{DETECTIONS}, and \code{MAX_SNR}.
#' @param detection_summary A data frame describing expected beacon detections. 
#' Must include columns: 
#' \code{TAG}, \code{BS}, \code{DETECTION_RATIO}, 
#' \code{OVERLAP_START}, \code{OVERLAP_END}, and \code{DISTANCE_M}.
#' @param min_detection_ratio Numeric. Minimum detection ratio threshold for 
#' considering a beacon–BS pair as expected (default = 0.5).
#' @param max_distance_beacon_bs_km Numeric. Maximum allowed distance (km) between 
#' beacon and BS for expected detections (default = 7). 
#' Currently not applied (commented out in code).
#' @param low_detection_fraction Numeric. Threshold for classifying a beacon 
#' as "low detection" if its normalized detection rate is below this fraction 
#' of the maximum possible (default = 0.6).
#'
#' @return A data frame with one row per BS and hour, including:
#' \describe{
#'   \item{BS}{Base station ID (character).}
#'   \item{HOUR}{Time in milliseconds since epoch (numeric).}
#'   \item{NUM_EXPECTED_BEACONS}{Number of beacons expected for that BS and hour.}
#'   \item{NUM_LOW_DETECTION_BEACONS}{Number of beacons with normalized detection below threshold.}
#'   \item{BEACONS_DETECTION_MULTIPLICATION}{Product of normalized detection fractions across beacons.}
#'   \item{BEACONS_DETECTION_RATE}{Total detections normalized by 
#'   (expected beacons × 3600).}
#'   \item{MAX_SNR_MEAN_RATIO}{Sum of maximum SNR values divided by number of expected beacons.}
#'   \item{TAGS_EXPECTED}{List of beacon IDs expected at that BS-hour.}
#' }
#'
#' @details 
#' - Timestamps (\code{HOUR}) are converted to POSIXct (\code{HOUR_POSIX}) 
#'   internally for overlap filtering, but the output keeps the original numeric hour.
#' - Normalized detections are computed as raw \code{DETECTIONS}/3600 (per-second rate).
#' - Expected detections are filtered by \code{min_detection_ratio} and constrained 
#'   to beacon–BS overlap times.
#'
#' @examples
#' \dontrun{
#' bs_summary <- read.csv("bs_summary_data.csv")
#' detection_summary <- read.csv("detection_summary.csv")
#' result <- generate_bs_hour_detection_rate_table_from_beacons(
#'   bs_summary, detection_summary, min_detection_ratio = 0.4
#' )
#' head(result)
#' }
#'
#' @import dplyr tidyr
#' @export
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
  
  return(bs_hour_summary)
}