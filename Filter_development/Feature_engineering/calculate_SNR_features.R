#' Calculate SNR-based features for each localization point
#'
#' This function computes summary statistics of the Signal-to-Noise Ratio (SNR) for each localization
#' based on the matched detections. Statistics include min, max, range, mean, median, standard deviation,
#' and coefficient of variation (CV) of the SNR values.
#'
#' @param matched_detections A data frame or data.table of detection records that have been matched to localizations.
#'                           Must include columns `TAG`, `TIME`, and `SNR`.
#' @param localizations_data A data frame or data.table containing the original localization data, with columns `TAG` and `TIME`.
#'
#' @return A data.frame of the localization data with additional columns for SNR features:
#' `SNR_min`, `SNR_max`, `SNR_range`, `SNR_mean`, `SNR_median`, `SNR_sd`, and `SNR_cv`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Groups the matched detections by `TAG` and `TIME`
#'   \item Calculates summary statistics of the `SNR` per group
#'   \item Merges the resulting features back into the localization data
#' }
#'
#' @examples
#' \dontrun{
#' snr_features <- calculate_SNR_features(matched_detections, localizations_data)
#' }
#'
#' @import data.table
#' @importFrom dplyr %>%
#' @export

library(data.table)
library(dplyr)

calculate_SNR_features <- function(matched_detections, localizations_data) {
  
  # Make sure both inputs are data.table for speed
  matched_dt <- as.data.table(matched_detections)
  loc_dt <- as.data.table(localizations_data)
  
  # Calculate SNR-based features per localization (by TAG and TIME or roundTIME)
  snr_summary <- matched_dt[
    , {
      snr_min <- min(SNR, na.rm = TRUE)
      snr_max <- max(SNR, na.rm = TRUE)
      snr_mean <- mean(SNR, na.rm = TRUE)
      snr_sd <- sd(SNR, na.rm = TRUE)
      list(
        SNR_min = snr_min,
        SNR_max = snr_max,
        SNR_range = snr_max - snr_min,
        SNR_mean = snr_mean,
        SNR_median = median(SNR, na.rm = TRUE),
        SNR_sd = snr_sd,
        SNR_cv = ifelse(snr_mean == 0, NA, snr_sd / snr_mean)
      )
    },
    by = .(TAG, TIME)
  ]
  
  # Merge SNR features back into the localization_data (preserving order)
  localizations_data_with_snr_features <- merge(loc_dt, snr_summary, by = c("TAG", "TIME"), all.x = TRUE)
  
  # Return as data.frame if needed
  return(as.data.frame(localizations_data_with_snr_features))
  
}