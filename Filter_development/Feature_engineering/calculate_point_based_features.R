# Calculate the point-based features per TAG and per Track_id

calculate_point_based_features <- function(localization_data, detection_data) {
  
  library(dplyr)
  
  source(file.path(getwd(), "atlas_metrics.R"))
  
  # Verify that the data frame has the columns TAG, track_id, and time_diff_sec
  if (!"TAG" %in% colnames(localization_data)) {
    stop("Error: 'TAG' column is missing from the dataframe.")
  }
  
  if (!"track_id" %in% colnames(localization_data)) {
    stop("Error: 'track_id' column is missing from the dataframe.")
  }
  
  if (!"time_diff_sec" %in% colnames(localization_data)) {
    stop("Error: 'time_diff_sec' column is missing from the dataframe.")
  }
  
  ### 1. Movement-Based Features (Time-Dependent)- to capture unrealistic movement patterns
  
  # Distance between consecutive points
  localization_data$dist_m <- calculate_distance(localization_data$X, 
                                                 localization_data$Y)
  
  # Speed [m/s]
  # was already calculated by the Visual Filter App
  
  # Acceleration [m/s^2]
  localization_data$Acceleration_m_s_2 <- calculate_acceleration(localization_data$Speed_m_s, 
                                                                 localization_data$time_diff_sec)
  
  # Cosine of the turning angle
  localization_data$cos_turning_angle <- calculate_cosine_turning_angle(localization_data$X, 
                                                                        localization_data$Y)   

  
  # Set the values of the beginning of the track to NA
  
  return(localization_data)
  
}