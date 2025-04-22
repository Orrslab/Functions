source(file.path(getwd(), "atlas_metrics.R"))

calculate_features_in_time_window <- function(window_data,
                                              X_observed_point,
                                              Y_observed_point) {
  
  speed_col <- window_data$Speed_m_s
  outliers_col <- window_data$Outliers
  valid_values <- outliers_col[!is.na(outliers_col)]
  
  list(
    X_median = calculate_median_of_column(window_data$X),
    Y_median = calculate_median_of_column(window_data$Y),
    Speed_window = calculate_std_of_column(speed_col),
    Speed_window_mean = calculate_mean_of_column(speed_col),
    Speed_window_median = calculate_median_of_column(speed_col),
    avg_dist_from_points_in_window = calculate_mean_of_column(calculate_euclidean_distance(window_data$X, X_observed_point, window_data$Y, Y_observed_point)),
    var_x_window = calculate_variance_of_column(window_data$X),
    var_y_window = calculate_variance_of_column(window_data$Y),
    outleirs_percentage = sum(valid_values == 1) / length(valid_values) * 100
  )

}