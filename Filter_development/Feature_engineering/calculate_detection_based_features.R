source(file.path(getwd(), "match_detections_to_localizations.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_SNR_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/load_and_format_base_stations_info.R"))
source(file.path(getwd(), "create_participating_base_stations_table.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_distance_to_closest_base_station.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_missed_base_stations_features.R"))

# Features per location, that reqire knowing the detections that correspond to each location
calculate_detection_based_features <- function(localizations_data, detections_data) {
  
  # Get the detections corresponding to each location
  matched_detections <- match_detections_to_localizations(localizations_data, detections_data)
  
  # Calculate the SNR features
  localizations_data_with_features <- calculate_SNR_features(matched_detections, localizations_data)
  
  # Load the base stations info
  base_stations_info_path <- "C:/Users/netat/Documents/Movement_Ecology/ATLAS/Base_stations_beacons_info/Base_stations_info.csv"
  base_stations_info <- load_and_format_base_stations_info(base_stations_info_path)
  
  # Calculate the distance of each location from the closest base station
  localizations_data_with_features <- calculate_distance_to_closest_base_station(
    localizations_data_with_features,
    matched_detections,
    base_stations_info)
  
  # Create a table with the participating base stations in each localization
  participating_base_stations <- create_participating_base_stations_table(localizations_data, matched_detections)
  
  # Get the base stations that were excluded from each localization 
  # and evaluated features that are related to these base stations
  results <- calculate_missed_base_stations_features(localizations_data_with_features,
                                                     base_stations_info)
  
  localizations_data_with_features <- results$localizations_data
  missed_base_stations <- results$missed_base_stations
  
  return(list(
    localizations_data = localizations_data_with_features,
    participating_base_stations = participating_base_stations,
    missed_base_stations = missed_base_stations
  ))
  
}