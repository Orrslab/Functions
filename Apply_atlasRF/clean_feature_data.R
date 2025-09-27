library(dplyr)

clean_feature_data <- function(localization_data, non_feature_column_names) {
  
  # Create a copy for later restoration of non-feature columns
  original_localization_data <- localization_data
  
  ##  Handle columns with structural NA values- random forest does not accept NA values
  
  # "closest_missed_bs_distance"
  localization_data$closest_missed_bs_distance_is_na <- localization_data$num_missed_bs == 0
  localization_data$closest_missed_bs_distance[is.na(localization_data$closest_missed_bs_distance)] <- 0
  
  # "mean_missed_bs_distance"
  localization_data$mean_missed_bs_distance_is_na <- localization_data$num_missed_bs < 2
  localization_data$mean_missed_bs_distance[localization_data$mean_missed_bs_distance_is_na] <- 0
  
  # Remove non-feature columns
  data_with_features_only <- localization_data %>%
    dplyr::select(-all_of(non_feature_column_names))
  
  # Identify rows with no NA values
  complete_rows <- complete.cases(data_with_features_only)
  
  # Report NA rows
  na_percentage <- mean(rowSums(is.na(data_with_features_only)) > 0) * 100
  print(paste("Percentage of rows with NA values:", na_percentage))

  # Return:
  list(
    clean_data_for_rf = data_with_features_only[complete_rows, ],
    full_localization_data = original_localization_data,
    complete_rows = complete_rows
  ) 
  
}