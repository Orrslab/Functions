library(ranger)

run_atlasRF_on_localization_data <- function(cleaned_data, rf_model, rf_prediction_threshold) {
  
  # Predict Outliers using the Random Forest model on clean data
  predicted_labels <- predict(rf_model, data = cleaned_data$clean_data_for_rf, type = "response")$predictions
  
  # Convert the probabilities that the model gives to labels
  predicted_labels <- ifelse(predicted_labels[, "outlier"] > rf_prediction_threshold, "outlier", "valid")
  
  # Prepare output vector filled with NAs
  outliers <- rep(NA, nrow(cleaned_data$full_localization_data))
  
  # Assign predicted labels to rows without NAs
  outliers[cleaned_data$complete_rows] <- predicted_labels
  
  # Add predicted Outliers column to full dataset
  cleaned_data$full_localization_data$Outliers <- outliers
  
  return(cleaned_data$full_localization_data)
}
