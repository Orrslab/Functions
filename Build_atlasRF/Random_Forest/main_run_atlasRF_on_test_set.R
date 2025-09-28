
library(ranger)
library(ggplot2)
library(precrec)   # For PR and ROC curves
library(pROC)      # For ROC AUC
library(caret)     # For confusion matrix
library(dplyr)

source(file.path(getwd(), "Build_atlasRF/Random_Forest/Performance_analysis/analyze_model_performance.R"))

#' Run a Trained atlasRF model on the Test Set and Analyze Performance
#'
#' This function loads a previously trained Random Forest model and a test dataset,
#' applies the model to predict outliers, and performs a comprehensive performance
#' analysis including threshold selection, confusion matrices, ROC/PR curves, and
#' per-species performance maps.
#'
#' @param config A list containing configuration settings for paths, model, and test data:
#'   - \code{config$paths$atlasRF_results_folder}: Folder path where model and datasets are stored.
#'   - \code{config$paths$filename_atlasRF_model_trained}: Filename of the trained Random Forest model (RDS format).
#'   - \code{config$paths$test_set_filename}: Filename of the test dataset (RDS format).
#'   - \code{config$training_and_test_settings$non_feature_column_names}: Character vector of columns to exclude from model features.
#'   - \code{config$atlasRF_test_settings$tree_vote_optional_thresholds}: Vector of thresholds to evaluate for classification.
#'
#' @details
#' The function performs the following steps:
#' 1. Loads the trained Random Forest model and the test dataset.
#' 2. Ensures reproducibility by setting a seed.
#' 3. Removes non-feature columns from the test dataset.
#' 4. Converts the label column \code{Outliers} to a factor with the same levels as used in training.
#' 5. Calls \code{analyze_model_performance()} to:
#'    - Select an optimal classification threshold.
#'    - Compute confusion matrices and classification metrics.
#'    - Generate ROC and Precision-Recall curves and save them as PNG.
#'    - Generate per-species and per-TAG performance maps.
#'
#' @return
#' The function does not return a value. All results, including plots, confusion matrices,
#' metrics tables, and maps, are saved in the specified \code{atlasRF_results_folder}.
#'
#' @examples
#' \dontrun{
#' config <- list(
#'   paths = list(
#'     atlasRF_results_folder = "results/",
#'     filename_atlasRF_model_trained = "rf_model_final.rds",
#'     test_set_filename = "test_data.rds"
#'   ),
#'   training_and_test_settings = list(
#'     non_feature_column_names = c("Species_id", "TAG", "Outliers")
#'   ),
#'   arlasRF_test_settings = list(
#'     tree_vote_optional_thresholds = seq(0.1, 0.9, by = 0.05)
#'   )
#' )
#' main_run_random_forest_on_test_set(config)
#' }
#'
#' @import ranger
#' @import ggplot2
#' @import precrec
#' @import pROC
#' @import caret
#' @import dplyr
#' @export
main_run_atlasRF_on_test_set <- function(config) {
  
  message("### RUNNING THE RANDOM FOREST MODEL ON THE TEST SET AND ANALYZING PERFORMANCE. ###")
  
  # Load trained model and test set
  rf_model_final <- readRDS(file.path(config$paths$atlasRF_results_folder, config$paths$filename_atlasRF_model_trained))
  test_data <- readRDS(file.path(config$paths$atlasRF_results_folder, config$paths$test_set_filename))
  
  # Ensure label is a factor with correct levels
  test_data$Outliers <- factor(test_data$Outliers, levels = rf_model_final$forest$levels)
  
  # Calibrate vote threshold and analyze model's performance
  analyze_model_performance(rf_model = rf_model_final, 
                            test_data = test_data,
                            non_feature_column_names = config$training_and_test_settings$non_feature_column_names,
                            tree_vote_optional_thresholds = config$atlasRF_test_settings$tree_vote_optional_thresholds,
                            random_forest_results_folder = config$paths$atlasRF_results_folder)
  
}

