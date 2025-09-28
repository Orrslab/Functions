# main script for RF model development for outliers filtering in the ATLAS data

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(caret)
library(ranger)
library(ggplot2)

# USER INPUT BEGIN

# Folder to save the model results
RF_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/Species_models/GJ"

training_set_filename <- "training_set.rds"

# USER INPUT END

# Load the training data from file
training_set <- readRDS(file.path(RF_results_folder, training_set_filename))

# Make sure that the Response Variable (Outliers) is of type factor
training_set$Outliers <- as.factor(training_set$Outliers)

# Define training control with 5-fold Cross-Validation (CV), stratified by class
train_control <- trainControl(
  method = "cv", 
  number = 5, 
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Set a grid of hyperparameters to try
tune_grid <- expand.grid(
  mtry = c(5, 10, 15),       # number of features to consider at each split
  splitrule = "gini",       # or "extratrees" if using ranger
  min.node.size = 1         # minimal node size
)

# To make the results reproducible across runs
set.seed(42)

# Train Random Forest with cross-validation
rf_model <- train(
  Outliers ~ .,  # Column name of the Response Variable
  data = training_set,
  method = "ranger",
  metric = "ROC",          # Use AUC to choose the best model
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity"
)

# View cross-validation results
print(rf_model)

print(rf_model$bestTune)               # Best hyperparameters
print(rf_model$results)                # All results (AUC, etc.)

# Variable importance
vip <- varImp(rf_model)
print(vip)

# Plot the features importance
vip_plot <- ggplot(vip, top = 20)

# Save the plot as a PNG using base graphics device
ggsave(filename = file.path(RF_results_folder, "variable_importance_plot.png"),
       plot = vip_plot,
       width = 8, height = 6, dpi = 300, bg = "white")

# Save the feature importance results as a csv
vip_df <- vip$importance
vip_df$Feature <- rownames(vip_df)
write.csv(vip_df[order(-vip_df$Overall), ], 
          file.path(RF_results_folder, "feature_importance.csv"), 
          row.names = FALSE)

# Plot the model
RF_plot <- ggplot(rf_model)
print(RF_plot)

# Save the plot
ggsave(file.path(RF_results_folder, "RF_model_performance.png"), plot = RF_plot, width = 8, height = 6, dpi = 300, bg = "white")

# CV predictions / performance metrics
saveRDS(rf_model$pred, file.path(RF_results_folder, "cv_predictions.rds"))
write.csv(rf_model$pred, file.path(RF_results_folder, "cv_predictions.csv"))

# Cross-validation summary results
write.csv(rf_model$results, file.path(RF_results_folder, "cv_results_summary.csv"))

# Save the model
saveRDS(rf_model, file.path(RF_results_folder, "rf_model.rds"))

# # Load the saved model
# rf_model <- readRDS(file.path(RF_results_folder, "rf_model.rds"))
