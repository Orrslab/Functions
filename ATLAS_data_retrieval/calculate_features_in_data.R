
library(dplyr)
library(data.table)

source(file.path(getwd(), "Filter_development/Feature_engineering", "calculate_point_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_time_window_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_post_window_features.R"))

calculate_features_in_data <- function(localization_data,
                                       detection_data,
                                       base_stations_info_path,
                                       beacons_detection_ratio_per_hour,
                                       base_stations_summary_per_beacon,
                                       low_beacon_detection_fraction,
                                       half_time_window_size_sec) {
  
  message("Calculating features:")
  
  # Calculate the point-based_features
  results <- calculate_point_based_features(localization_data, 
                                            detection_data, 
                                            base_stations_info_path,
                                            beacons_detection_ratio_per_hour,
                                            base_stations_summary_per_beacon,
                                            low_beacon_detection_fraction)
  
  # Extract the results
  localization_data <- results$localization_data
  
  # Calculate the time-window-based features
  localization_data <- calculate_time_window_based_features(localization_data = localization_data,
                                                            half_window_size_sec = half_time_window_size_sec)
  
  # Calculate features that require values of other point-based and window-based features
  localization_data <- calculate_post_window_features(localization_data)
  
  return(localization_data)
  
}