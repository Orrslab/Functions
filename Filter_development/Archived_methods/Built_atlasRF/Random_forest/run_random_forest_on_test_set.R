
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(ranger)
library(ggplot2)
library(precrec)   # For PR and ROC curves
library(pROC)      # For ROC AUC
library(caret)     # For confusion matrix
library(dplyr)

source(file.path(getwd(), "Filter_development/Random_Forest/Performance_analysis/analyze_model_performance.R"))

# USER'S INPUT BEGIN
random_forest_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/Species_models/WB"
rf_model_filename <- "rf_model_final_trained_on_full_training_set.rds"

# Remove Stops flag
remove_stops_from_test_set <- FALSE

# Define the columns that are not considered as features and should be excluded from the training set
non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "dateTime", "DAY", "geometry",
                              "Species_id", "X_mean", "Y_mean", "X_median", "Y_median", "Is_stop")

# USER'S INPUT END

# Load trained model and test set
rf_model_final <- readRDS(file.path(random_forest_results_folder, rf_model_filename))
test_data <- readRDS(file.path(random_forest_results_folder, "test_set.rds"))

# To make the results reproducible across runs
set.seed(42)

# Remove stops from the test set
if (remove_stops_from_test_set) {
  if ("Is_stop" %in% colnames(test_data)) {
    test_data <- test_data[test_data$Is_stop != 1, ]
  }
}

# Remove the non-feature columns from the training set
features_only <- test_data %>%
  dplyr::select(-all_of(non_feature_column_names))

# Ensure label is a factor with correct levels
test_data$Outliers <- factor(test_data$Outliers, levels = rf_model_final$forest$levels)

# Calibrate vote threshold and analyze model's performance
analyze_model_performance(rf_model = rf_model_final, test_tata = test_data)

