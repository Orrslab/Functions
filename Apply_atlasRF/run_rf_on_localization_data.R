library(ranger)

source(file.path(getwd(), "ATLAS_data_retrieval/clean_feature_data.R"))

run_rf_on_localization_data <- function(localization_data, non_feature_column_names, rf_model, rf_prediction_threshold) {
  
  # Clean the data to prepare it for the Random Forest model
  results <- clean_feature_data(localization_data, non_feature_column_names)
  
  # Predict Outliers using the Random Forest model on clean data
  predicted_labels <- predict(rf_model, data = results$clean_data_for_rf, type = "response")$predictions
  
  # Convert the probabilities that the model gives to labels
  predicted_labels <- ifelse(predicted_labels[, "outlier"] > rf_prediction_threshold, "outlier", "valid")
  
  # Prepare output vector filled with NAs
  outliers <- rep(NA, nrow(results$full_localization_data))
  
  # Assign predicted labels to rows without NAs
  outliers[results$complete_rows] <- predicted_labels
  
  # Add predicted Outliers column to full dataset
  results$full_localization_data$Outliers <- outliers
  
  return(results$full_localization_data)
}
