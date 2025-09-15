
library(caret)
library(ranger)
library(ggplot2)

#' @title Hyperparameter Tuning of Random Forest for Outlier Detection
#'
#' @description
#' This function trains and tunes a Random Forest classifier using the 
#' `caret` and `ranger` packages to filter outliers in ATLAS animal 
#' movement data. It performs 5-fold cross-validation, evaluates 
#' model performance, extracts feature importance, and saves 
#' results and plots to the configured output folder.
#'
#' @param config A list containing configuration parameters, including:
#' \describe{
#'   \item{config$paths$random_forest_results_folder}{Path to the folder where results will be saved.}
#'   \item{config$paths$training_set_filename}{Filename of the RDS file with the training dataset.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Loads the training dataset from the specified file.
#'   \item Ensures the response variable \code{Outliers} is a factor.
#'   \item Performs hyperparameter tuning with 5-fold cross-validation:
#'         \code{mtry} (5, 10, 15), \code{splitrule = "gini"}, 
#'         \code{min.node.size = 1}.
#'   \item Selects the best model based on AUC (ROC metric).
#'   \item Extracts and plots variable importance.
#'   \item Saves model, cross-validation results, feature importance, 
#'         and performance plots as RDS, CSV, and PNG files.
#' }
#'
#' @return 
#' The function has no direct return value. It produces side effects by 
#' saving trained models, evaluation metrics, and plots into the 
#' configured results folder.
#'
#' @examples
#' \dontrun{
#'   config <- list(
#'     paths = list(
#'       random_forest_results_folder = "results/",
#'       training_set_filename = "training_set.rds"
#'     )
#'   )
#'   main_tune_hyper_parameters_of_random_forest(config)
#' }
#'
#' @seealso \code{\link[caret]{train}}, \code{\link[ranger]{ranger}}, \code{\link[caret]{trainControl}}
#'
#' @export
main_tune_hyper_parameters_of_random_forest <- function(config) {
  
  message("### STARTED THE MAIN SCRIPT OF HYPER PARAMETER TUNNING FOR THE RANDOM FOREST MODEL. ###")
  
  # Load the training data from file
  training_set <- readRDS(file.path(config$paths$random_forest_results_folder, config$paths$training_set_filename))
  
  # Make sure that the Response Variable (Outliers) is of type factor
  training_set$Outliers <- as.factor(training_set$Outliers)
  
  # Define training control with 5-fold Cross-Validation (CV), stratified by class
  train_control <- trainControl(
    method = "cv", 
    number = config$random_forest_hyperparameter_settings$number_of_cross_validation_folds, 
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
  # Set a grid of hyperparameters to try
  tune_grid <- expand.grid(
    mtry = config$random_forest_hyperparameter_settings$mtry_values,   # number of features to consider at each split
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
  ggsave(filename = file.path(config$paths$random_forest_results_folder, "variable_importance_plot.png"),
         plot = vip_plot,
         width = 8, height = 6, dpi = 300, bg = "white")
  
  # Save the feature importance results as a csv
  vip_df <- vip$importance
  vip_df$Feature <- rownames(vip_df)
  write.csv(vip_df[order(-vip_df$Overall), ], 
            file.path(config$paths$random_forest_results_folder, "feature_importance.csv"), 
            row.names = FALSE)
  
  # Plot the model
  RF_plot <- ggplot(rf_model)
  print(RF_plot)
  
  # Save the plot
  ggsave(file.path(config$paths$random_forest_results_folder, "RF_model_performance.png"), plot = RF_plot, width = 8, height = 6, dpi = 300, bg = "white")
  
  # CV predictions / performance metrics
  saveRDS(rf_model$pred, file.path(config$paths$random_forest_results_folder, "cv_predictions.rds"))
  write.csv(rf_model$pred, file.path(config$paths$random_forest_results_folder, "cv_predictions.csv"))
  
  # Cross-validation summary results
  write.csv(rf_model$results, file.path(config$paths$random_forest_results_folder, "cv_results_summary.csv"))
  
  # Save the model
  saveRDS(rf_model, file.path(config$paths$random_forest_results_folder, config$paths$filename_random_forest_model_tuned))
  
}

