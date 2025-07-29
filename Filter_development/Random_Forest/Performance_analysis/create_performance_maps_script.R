library(readr)

source(file.path(getwd(), "Filter_development/Random_Forest/Performance_analysis/create_and_save_performance_maps.R"))

# USER'S INPUT BEGIN
random_forest_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/GJ_29-07-25"
rf_model_filename <- "rf_model_final_trained_on_full_training_set.rds"

# Define the columns that are not considered as features and should be excluded from the training set
non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "dateTime", "DAY", "geometry",
                              "Species_id", "X_mean", "Y_mean", "X_median", "Y_median", "Is_stop")

# Set threshold for the labels prediction
threshold <- 0.9

# USER'S INPUT END

# # Load trained model and test set
# rf_model_final <- readRDS(file.path(random_forest_results_folder, rf_model_filename))
# test_data <- readRDS(file.path(random_forest_results_folder, "test_set.rds"))
# 
# # Remove the non-feature columns from the training set
# features_only <- test_data %>%
#   dplyr::select(-all_of(non_feature_column_names))
# 
# # Ensure label is a factor with correct levels
# test_data$Outliers <- factor(test_data$Outliers, levels = rf_model_final$forest$levels)
# 
# # Predict class probabilities
# pred_probs <- predict(rf_model_final, data = features_only, type = "response")$predictions
# 
# # If the probability for "outlier" class is larger than the threshold, set the class as "outlier"
# pred_classes <- ifelse(pred_probs[, "outlier"] > threshold, "outlier", "valid")

#### Generate maps that show the model performance
# Add the predictions to the test data
# test_data$predicted_outlier <- ifelse(pred_probs[, "outlier"] > threshold, "outlier", "valid")
# Add predicted labels
test_data$predicted_outlier <- factor(pred_classes, levels = c("outlier", "valid"))

# Assuming test_data$Outliers and test_data$predicted_outlier are factors with levels c("outlier", "valid")

test_data$confusion_category <- NA  # יצירת עמודה ריקה

test_data$confusion_category[test_data$Outliers == "outlier" & test_data$predicted_outlier == "outlier"] <- "True Positive"
test_data$confusion_category[test_data$Outliers == "valid"   & test_data$predicted_outlier == "valid"]   <- "True Negative"
test_data$confusion_category[test_data$Outliers == "valid"   & test_data$predicted_outlier == "outlier"] <- "False Positive"
test_data$confusion_category[test_data$Outliers == "outlier" & test_data$predicted_outlier == "valid"]   <- "False Negative"

# הפיכת העמודה לפקטור עם סדר הגיוני (לא חובה אבל מומלץ)
test_data$confusion_category <- factor(test_data$confusion_category,
                                       levels = c("True Positive", "True Negative", "False Positive", "False Negative"))


# Create performance maps per species and TAG
create_and_save_performance_maps(
  data = test_data,
  color_column = "confusion_category",
  output_dir = file.path(random_forest_results_folder, "maps_by_species_and_tag")
)