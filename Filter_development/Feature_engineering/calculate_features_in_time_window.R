source(file.path(getwd(), "atlas_metrics.R"))

#' Calculate Features in a Time Window
#'
#' Computes summary statistics and spatial metrics for a given subset of localizations (a time window)
#' centered around a certain 'observed' location point.
#'
#' @param window_data A `data.frame` or `data.table` containing localization data for a specific time window.
#' It must include columns: `X`, `Y`, `Speed_m_s`, and `Outliers`.
#' @param X_observed_point Numeric. The X-coordinate of the observed location point (the center point of the time window).
#' @param Y_observed_point Numeric. The Y-coordinate of the observed location point.
#'
#' @return A named list of computed features, including:
#' \itemize{
#'   \item `X_mean`, `Y_mean`: Mean of X and Y coordinates in the window.
#'   \item `X_median`, `Y_median`: Median of X and Y coordinates.
#'   \item `Speed_window_std`, `Speed_window_mean`, `Speed_window_median`: Standard deviation, mean, and median of speeds in the window.
#'   \item `avg_dist_from_points_in_window`: Mean Euclidean distance from the observed location point to all points in the window.
#'   \item `var_x_window`, `var_y_window`: Variance of X and Y coordinates.
#'   \item `outleirs_percentage`: Percentage of localizations marked as outliers within the window (value = 1 in `Outliers` column).
#' }
#'
#' @details
#' This function is intended to support time-window-based feature extraction for animal movement analysis.
#' It uses helper functions like \code{calculate_mean_of_column()}, \code{calculate_variance_of_column()}, and
#' \code{calculate_euclidean_distance()} assumed to be defined in the sourced \code{atlas_metrics.R} file.
#'
#' @seealso \code{\link{calculate_time_window_based_features}} for looping over all localizations using this function.
#'
#' @examples
#' \dontrun{
#' features <- calculate_features_in_time_window(window_data, 452300.12, 3651200.85)
#' }
#'
#' @export

calculate_features_in_time_window <- function(window_data,
                                              X_observed_point,
                                              Y_observed_point) {
  
  speed_col <- window_data$Speed_m_s
  outliers_col <- window_data$Outliers
  valid_values <- outliers_col[!is.na(outliers_col)]
  
  list(
    X_mean = calculate_mean_of_column(window_data$X),
    Y_mean = calculate_mean_of_column(window_data$Y),
    X_median = calculate_median_of_column(window_data$X),
    Y_median = calculate_median_of_column(window_data$Y),
    Speed_window_std = calculate_std_of_column(speed_col),
    Speed_window_mean = calculate_mean_of_column(speed_col),
    Speed_window_median = calculate_median_of_column(speed_col),
    avg_dist_from_points_in_window = calculate_mean_of_column(calculate_euclidean_distance(window_data$X, X_observed_point, window_data$Y, Y_observed_point)),
    var_x_window = calculate_variance_of_column(window_data$X),
    var_y_window = calculate_variance_of_column(window_data$Y),
    outleirs_percentage = sum(valid_values == 1) / length(valid_values) * 100
  )

}