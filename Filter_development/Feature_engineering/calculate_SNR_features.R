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