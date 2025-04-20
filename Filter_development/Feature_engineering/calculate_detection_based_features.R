source(file.path(getwd(), "match_detections_to_localizations.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_SNR_features.R"))

# Features per location, that reqire knowing the detections that correspond to each location
calculate_detection_based_features <- function(localizations_data, detections_data) {
  
  # Get the detections corresponding to each location
  machted_detections <- match_detections_to_localizations(localizations_data, detections_data)
  
  localizations_data_with_features <- calculate_SNR_features(machted_detections, localizations_data)
  
  return(localizations_data_with_features)
  
}