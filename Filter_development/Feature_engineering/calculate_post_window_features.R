source(file.path(getwd(), "atlas_metrics.R"))

#' Calculate Post-Window Features for Localizations
#'
#' Adds derived features to localization data based on comparisons between observed points and
#' summary statistics from a surrounding time window (mean/median coordinates and speeds).
#'
#' @param localization_data A `data.frame` or `data.table` containing localization data with precomputed
#' window-based features, such as `X_mean`, `X_median`, `Speed_window_mean`, and `Speed_window_median`.
#'
#' @return A `data.frame` or `data.table` (same class as input) with the following additional columns:
#' \itemize{
#'   \item `dist_from_mean`: Euclidean distance between observed point and window mean location.
#'   \item `dist_from_meadian`: Euclidean distance between observed point and window median location.
#'   \item `diff_dist_mean_median`: Absolute difference between the two distances above.
#'   \item `ratio_dist_mean_median`: Ratio between distance from mean and distance from median location.
#'   \item `speed_diff_from_mean`: Absolute difference between observed speed and mean speed in window.
#'   \item `speed_diff_from_median`: Absolute difference between observed speed and median speed in window.
#'   \item `speed_z_score`: Z-score of observed speed relative to window mean and standard deviation.
#'   \item `ratio_speed_median`: Ratio between observed speed and median speed in window.
#' }
#'
#' @details
#' This function assumes that \code{calculate_euclidean_distance()} has been defined and sourced
#' from the \code{atlas_metrics.R} script. It should be applied after window-based summary features
#' have already been calculated (e.g., via \code{calculate_time_window_based_features()}).
#'
#' @seealso
#' \code{\link{calculate_time_window_based_features}}, \code{\link{calculate_features_in_time_window}}
#'
#' @examples
#' \dontrun{
#' enriched_data <- calculate_post_window_features(localization_data)
#' }
#'
#' @export

calculate_post_window_features <- function(localization_data) {
  
  ## Distance of the observed point from the mean location in window
  localization_data$dist_from_mean <- calculate_euclidean_distance(localization_data$X,
                                                                   localization_data$X_mean,
                                                                   localization_data$Y,
                                                                   localization_data$Y_mean)
  
  ## Distance of the observed point from the median location in window
  localization_data$dist_from_meadian <- calculate_euclidean_distance(localization_data$X,
                                                                      localization_data$X_median,
                                                                      localization_data$Y,
                                                                      localization_data$Y_median)
  
  ## Difference between the distance from the mean and median locations
  localization_data$diff_dist_mean_median <- abs(localization_data$dist_from_mean - localization_data$dist_from_meadian)
  
  ## Ratio between the distance from the mean and median locations
  localization_data$ratio_dist_mean_median <- ifelse(
    localization_data$dist_from_meadian == 0,
    NA,
    localization_data$dist_from_mean / localization_data$dist_from_meadian
  )
  
  ## Difference between the speed of the observed point to the mean speed in window
  localization_data$speed_diff_from_mean <- abs(localization_data$Speed_m_s - localization_data$Speed_window_mean)
  
  ## Distance between the speed of the observed point to the median speed in window
  localization_data$speed_diff_from_median <- abs(localization_data$Speed_m_s - localization_data$Speed_window_median)
  
  ## Z-score of each point's speed
  localization_data$speed_z_score <- ifelse(
    is.na(localization_data$Speed_window_std) | localization_data$Speed_window_std == 0,
    NA,
    localization_data$speed_diff_from_mean / localization_data$Speed_window_std
  )
  
  ## Ratio to median speed
  localization_data$ratio_speed_median <- ifelse(
    is.na(localization_data$Speed_m_s) | is.na(localization_data$Speed_window_median) | localization_data$Speed_window_median == 0,
    NA,
    localization_data$Speed_m_s / localization_data$Speed_window_median
  )
  
  return(localization_data)
}