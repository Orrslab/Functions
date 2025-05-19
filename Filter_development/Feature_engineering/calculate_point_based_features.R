library(dplyr)

source(file.path(getwd(), "atlas_metrics.R"))
source(file.path(getwd(), "calculate_elevation_per_location.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_detection_based_features.R"))

# Calculate the point-based features per TAG and TIME

calculate_point_based_features <- function(localization_data, detection_data) {
  
  # Verify that the data frame has the columns TAG and time_diff_sec
  if (!"TAG" %in% colnames(localization_data)) {
    stop("Error: 'TAG' column is missing from the dataframe.")
  }
  
  if (!"time_diff_sec" %in% colnames(localization_data)) {
    stop("Error: 'time_diff_sec' column is missing from the dataframe.")
  }
  
  ### 1. Movement-Based Features (Time-Dependent)- to capture unrealistic movement patterns
  
  # Distance between consecutive points
  localization_data$dist_m <- calculate_distance(localization_data$X, 
                                                 localization_data$Y)
  
  # For each location point, calculate the ratio between the distance from the previous point and the distance between the previous and next point
  localization_data$distance_triangle_ratio <- calculate_triangle_distance_ratio(localization_data$X,
                                                                                 localization_data$Y)
  
  # Speed [m/s]
  # was already calculated by the Visual Filter App
  
  # Acceleration [m/s^2]
  localization_data$Acceleration_m_s_2 <- calculate_acceleration(localization_data$Speed_m_s, 
                                                                 localization_data$time_diff_sec)
  
  # Cosine of the turning angle
  localization_data$cos_turning_angle <- calculate_cosine_turning_angle(localization_data$X, 
                                                                        localization_data$Y)   

  ### 2. Location-Based Features
  # Currently I calculate these features only within a time window
  
  ### 3. Signal-Based Features
  
  # Elevation above sea level from Digital Elevation Model (DEM)
  localization_data <- calculate_elevation_per_location(localization_data,
                                                        dem_file = "DEM_Harod.tif")
  
  # NBS- Number of participation Base Stations- already included in the raw data from the ATLAS database

  # SNR- Signal to Noise Ratio and other detection-based features
  
  results <- calculate_detection_based_features(localization_data, detection_data)
  
  return(list(
    localizations_data = results$localizations_data,
    participating_base_stations = results$participating_base_stations,
    missed_base_stations = results$missed_base_stations
  ))
  
}