source(file.path(getwd(), "atlas_metrics.R"))

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