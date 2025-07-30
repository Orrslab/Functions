# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(ranger)

# USER'S INPUT BEGIN
random_forest_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/GJ_29-07-25"
# USER'S INPUT END

# Load the training dataset
train_data <- readRDS(file.path(random_forest_results_folder, "training_set.rds"))

# Make sure the label column is a factor type (required for running the random forest)
train_data$Outliers <- as.factor(train_data$Outliers)

# # Load caret model from tuning stage
# caret_model <- readRDS(file.path(random_forest_results_folder, "rf_model.rds"))
# 
# # Extract the best hyperparameters from the validated model
# best_mtry <- caret_model$bestTune$mtry
# best_splitrule <- caret_model$bestTune$splitrule
# best_min_node_size <- caret_model$bestTune$min.node.size

# Extract the best hyperparameters from the validated model
best_mtry <- 10
best_splitrule <- "gini"
best_min_node_size <- 1

# Train the final model on the full training dataset
rf_model_final <- ranger(
  formula = Outliers ~ .,
  data = train_data,
  mtry = best_mtry,
  splitrule = best_splitrule,
  min.node.size = best_min_node_size,
  probability = TRUE,
  importance = "impurity",
  seed = 42
)


# Save the trained model to file
saveRDS(rf_model_final, file.path(random_forest_results_folder, "rf_model_final_trained_on_full_training_set.rds"))

# Print model summary
print(rf_model_final)
