# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(ranger)
library(ggplot2)
library(readr)
library(precrec)   # For PR and ROC curves
library(pROC)      # For ROC AUC
library(caret)     # For confusion matrix
library(dplyr)

source(file.path(getwd(), "Filter_development/Random_Forest/create_and_save_performance_maps.R"))

# USER'S INPUT BEGIN
random_forest_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/All_species_28-07-25"

# Remove Stops flag
remove_stops_from_test_set <- TRUE

# Define the columns that are not considered as features and should be excluded from the training set
non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "dateTime", "DAY", "geometry",
                              "Species_id", "X_mean", "Y_mean", "X_median", "Y_median", "Is_stop")

# non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
#                               "TIME", "dateTime", "DAY", "geometry")
# USER'S INPUT END

# Load trained model and test set
rf_model_final <- readRDS(file.path(random_forest_results_folder, "rf_model_final_trained_on_full_training_set.rds"))
test_data <- readRDS(file.path(random_forest_results_folder, "test_set.rds"))

# To make the results reproducible across runs
set.seed(42)

# Get Outliers distribution in the raw test set
label_probs <- prop.table(table(test_data$Outliers))

# Sample points randomly while keeping the Outliers distribution the same as 
# the distribution of the raw test data
test_data$Outliers <- sample(names(label_probs),
                             size = nrow(test_data),
                             replace = TRUE,
                             prob = label_probs)

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

# Predict class probabilities
pred_probs <- predict(rf_model_final, data = features_only, type = "response")$predictions

# If label has two levels, extract probability for the "positive" class ("outlier")
positive_class <- levels(test_data$Outliers)[1]
probs_positive <- pred_probs[, positive_class]

# Create a precrec object to evaluate both PR and ROC curves
eval <- evalmod(scores = probs_positive, labels = test_data$Outliers)

# Plot and show the curves
autoplot(eval)

# Save the plot as PNG
ggsave(file.path(random_forest_results_folder, "Precision-Recall_curve.png"), width = 8, height = 6, dpi = 300)

# Compute and plot ROC manually with pROC for extra control
roc_obj <- roc(response = test_data$Outliers,
               predictor = pred_probs[, "outlier"],
               levels = c("outlier", "valid"),
               direction = ">")

plot(roc_obj, main = "ROC Curve (pROC)")

# Save the ROC curve as a png file
png(file.path(random_forest_results_folder, "ROC_curve_pROC.png"),
    width = 800, height = 600)
plot(roc_obj, main = "ROC Curve (pROC)")
dev.off()

cat("ROC AUC:", auc(roc_obj), "\n")

# Set threshold for the confusion table
threshold <- 0.9

# If the probability for "outlier" class is larger than the threshold, set the class as "outlier"
pred_classes <- ifelse(pred_probs[, "outlier"] > threshold, "outlier", "valid")

# Convert to factor
pred_classes <- factor(pred_classes, levels = c("outlier", "valid"))

# Evaluate the confusion matrix of the predictions
conf_mat <- confusionMatrix(pred_classes, test_data$Outliers, positive = "outlier")
print(conf_mat)

# Get the confusion table without all the metrics
conf_table <- as.data.frame(conf_mat$table)

# Save the confusion table as csv
write.csv(conf_table, file = file.path(random_forest_results_folder, "confusion_matrix.csv"), row.names = FALSE)

# Get confusion table metrics (Accuracy, Sensitivity / Recall, Specificity, Precision, F1-score) and save as csv
# Convert overall and byClass metrics to single-row data frames
overall_df <- as.data.frame(t(conf_mat$overall))
byClass_df <- as.data.frame(t(conf_mat$byClass))

# Ensure both data frames have the same columns
missing_cols_in_overall <- setdiff(colnames(byClass_df), colnames(overall_df))
missing_cols_in_byClass <- setdiff(colnames(overall_df), colnames(byClass_df))

# Add missing columns with NA values
for (col in missing_cols_in_overall) {
  overall_df[[col]] <- NA
}
for (col in missing_cols_in_byClass) {
  byClass_df[[col]] <- NA
}

# Reorder columns to match before combining
overall_df <- overall_df[, sort(colnames(overall_df))]
byClass_df <- byClass_df[, sort(colnames(byClass_df))]

# Add a column to indicate the type of metrics
overall_df$Metric_Type <- "Overall"
byClass_df$Metric_Type <- "ByClass"

# Combine both metric sets into one data frame
metrics_df <- rbind(overall_df, byClass_df)

# Save the combined metrics to CSV
write.csv(metrics_df, file = file.path(random_forest_results_folder, "classification_metrics.csv"), row.names = FALSE)
