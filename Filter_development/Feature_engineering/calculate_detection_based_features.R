source(file.path(getwd(), "match_detections_to_localizations.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_SNR_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_distance_to_closest_base_station.R"))

# Features per location, that reqire knowing the detections that correspond to each location
calculate_detection_based_features <- function(localizations_data, detections_data) {
  
  # Get the detections corresponding to each location
  matched_detections <- match_detections_to_localizations(localizations_data, detections_data)
  
  # Calculate the SNR features
  localizations_data_with_features <- calculate_SNR_features(matched_detections, localizations_data)
  
  # Calculate the distance of each location from the closest receiver
  localizations_data_with_features <- calculate_distance_to_closest_base_station(
    matched_detections,
    localizations_data_with_features)
  
  return(localizations_data_with_features)
  
}