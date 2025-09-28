# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3verse)
library(mlr3pipelines)
library(paradox)
library(data.table)

### USER'S INPUT BEGIN ###
# Folder of the Random Forest results
RF_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/All_species_08-06-25"

training_set_filename <- "training_set.rds"
test_set_filename <- "test_set.rds"

# Number of combinations to try
num_combinations_to_try <- 1
### USER'S INPUT END ###

# Load training set
training_set <- readRDS(file.path(RF_results_folder, training_set_filename))

# Make sure Outliers is a factor (for classification)
training_set$Outliers <- as.factor(training_set$Outliers)

# Define task
task <- TaskClassif$new(id = "rf_outliers", backend = training_set, target = "Outliers", positive = "outlier")

# Define learner (Random Forest via ranger)
learner <- lrn("classif.ranger", predict_type = "prob", importance = "impurity")

# Define search space for hyperparameter tuning
search_space <- paradox::ps(
  mtry = paradox::p_int(lower = 2, upper = ncol(training_set) - 1),
  min.node.size = paradox::p_int(lower = 1, upper = 20),
  splitrule = paradox::p_fct(levels = c("gini", "extratrees"))
)

# Define resampling method (5-fold CV)
resampling <- rsmp("cv", folds = 5)

# Define performance metric (AUC is a good default)
measure <- msr("classif.auc")

# Define tuner (random search – can switch to grid or bayesopt later)
# random_search means that in each tunning iteration, 
# the hyperparameters combinations will be selected randomly
# grid_search goes over all the hyperparameters systematically- which is slower but more accurate
# bayespot- bayesian search- uses previous results to choose the next hyperparametes combination
tuner <- tnr("random_search")
# tuner <- tnr("grid_search")
# tuner <- tnr("bayesopt")

# Define AutoTuner
at <- AutoTuner$new(
  learner = learner,
  resampling = resampling,
  measure = measure,
  search_space = search_space,
  terminator = trm("evals", n_evals = num_combinations_to_try), 
  tuner = tuner
)

# Train tuned model
at$train(task)

# View best hyperparameters
print(at$learner$model$param_set$values)

# Save tuned model to disk
saveRDS(at, file.path(RF_results_folder, "tuned_rf_model_mlr3.rds"))

# Get feature importance
importance <- at$learner$model$variable.importance

# Sort the importance by decreasing order
importance_sorted <- sort(importance, decreasing = TRUE)

# print the feature importance
print(importance_sorted)

# Save the feature importance
write.csv(importance_sorted, file = file.path(RF_results_folder, "feature_importance.csv"))

# Save a comparison table with the performance of each random forest model (combination of hyperparameters)
write.csv(at$archive$data(), "tuning_results.csv", row.names = FALSE)
