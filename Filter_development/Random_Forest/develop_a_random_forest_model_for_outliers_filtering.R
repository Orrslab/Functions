# main script for RF model development for outliers filtering in the ATLAS data

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(caret)
library(ggplot2)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/split_training_test_data_stratified.R"))

# USER INPUT BEGIN
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
features_data_filename <- "Features_data_for_RF_all_species.sqlite"

# Which fraction of the features' you want to use for training + validation of the random forest model?
training_valiation_data_fraction <- 0.8

# Folder to save the model results
RF_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/All_species"

training_validation_set_filename <- "training_and_validation_set.rds"
test_set_filename <- "test_set.rds"

# USER INPUT END

# features_filepath <- file.path(path_to_data_with_features, features_data_filename)
# 
# # Load the features data from sqlite
# features_data <- load_tables_from_sqlite_file(
#   sqlite_filepath = features_filepath,
#   tables = "LOCALIZATIONS")
# 
# localizations_data <- features_data$LOCALIZATIONS
# 
# # Split the data into a trainig+validation set and a test set
# data_sets <- split_training_test_data_stratified(data = localizations_data,
#                                                  train_frac = training_valiation_data_fraction)
# 
# training_and_validation_set <- data_sets$training_and_validation_set
# test_set <- data_sets$test_set
# 
# # save the datasets
# saveRDS(training_and_validation_set, file = file.path(RF_results_folder, training_validation_set_filename))
# saveRDS(test_set, file = file.path(RF_results_folder, test_set_filename))

# Load the training data from file
training_and_validation_set <- readRDS(file.path(RF_results_folder, training_validation_set_filename))

# REMOVE THE FEATURE "outleirs_percentage"
training_and_validation_set <- training_and_validation_set %>% select(-outliers_percentage)

# Make sure that the Response Variable (Outliers) is of type factor
training_and_validation_set$Outliers <- as.factor(training_and_validation_set$Outliers)

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
  mtry = c(2, 5, 10),       # number of features to consider at each split
  splitrule = "gini",       # or "extratrees" if using ranger
  min.node.size = 1         # minimal node size
)

# Train Random Forest with cross-validation
rf_model <- train(
  Outliers ~ .,  # Column name of the Response Variable
  data = training_and_validation_set,
  method = "ranger",
  metric = "ROC",          # Use AUC to choose the best model
  trControl = train_control,
  tuneGrid = tune_grid
)

# View cross-validation results
print(rf_model)

print(rf_model$bestTune)               # Best hyperparameters
print(rf_model$results)                # All results (AUC, etc.)

# Variable importance
print(varImp(rf_model))

# Save the model
saveRDS(rf_model, file.path(RF_results_folder, "rf_model.rds"))

# Load the saved model
rf_model <- readRDS(file.path(RF_results_folder, "rf_model.rds"))

# Plot the model
RF_plot <- ggplot(rf_model)
print(RF_plot)

# Save the plot
ggsave(file.path(RF_results_folder, "RF_model_performance.png"), plot = RF_plot, width = 8, height = 6, dpi = 300, bg = "white")
