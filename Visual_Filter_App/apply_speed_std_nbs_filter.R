#' Apply Speed, Standard Deviation, and Number of Base Stations Filter
#'
#' This function applies a baseline filter on the speed- in meters per second, standard deviation, and number of base stations
#' of the provided data to identify potential outliers. It calculates speed and standard deviation
#' metrics, then uses pre-defined thresholds to filter the data.
#'
#' @param data_for_filter A data frame containing the data to be filtered. It must include the following columns:
#'   - \code{TIME}: Timestamps of the data points.
#'   - \code{X}, \code{Y}, \code{Z}: Spatial coordinates of the data points.
#'   - \code{VARX}, \code{VARY}, \code{COVXY}: Variance and covariance values required for standard deviation calculation.
#'
#' @return A data frame with the same structure as \code{data_for_filter}, but with additional columns:
#'   - \code{Speed_m_s}: Calculated speed in meters per second.
#'   - \code{STD}: Calculated standard deviation.
#'   The data frame is updated to reflect the application of the baseline filter.
#'
#' @details
#' This function relies on the following external scripts:
#' \code{atlas_metrics.R} for metric calculations and \code{baseline_filter.R} for the filtering logic.
#' Ensure these scripts are available in the \code{path_to_atlas_data_analysis_repo} directory.
#'
#' @export
apply_speed_std_nbs_filter <- function(data_for_filter,
                                       speed_threshold_baseline_filter = 20,
                                       std_threshold_baseline_filter = 15,
                                       nbs_threshold_baseline_filter = 3) {
  
  # speed_threshold_baseline_filter is in [m/s]
  
  source(paste0(getwd(), "/atlas_metrics.R"))
  
  time_diff_s <- calculate_time_diff(data_for_filter$TIME)
  
  distance_m <- calculate_distance(data_for_filter$X, data_for_filter$Y)
  
  data_for_filter$Speed_m_s <- calculate_speed(distance_m, time_diff_s)
  
  data_for_filter$STD <- calculate_std(data_for_filter$VARX, data_for_filter$VARY, data_for_filter$COVXY)
  
  source(paste0(getwd(), "/baseline_filter.R"))
  data_for_filter <- baseline_filter(data_for_filter,
                                     speed_threshold_baseline_filter,
                                     std_threshold_baseline_filter,
                                     nbs_threshold_baseline_filter)
  
  
  return(data_for_filter)
  
}
