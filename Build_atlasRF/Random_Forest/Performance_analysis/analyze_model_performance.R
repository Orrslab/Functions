
source(file.path(getwd(), "Build_atlasRF/Random_Forest/Performance_analysis/threshold_analysis.R"))
source(file.path(getwd(), "Build_atlasRF/Random_Forest/Performance_analysis/performance_mapping_pipeline.R"))

#' Analyze Random Forest Model Performance on a Test Set
#'
#' This function evaluates a trained Random Forest model on a test dataset. It predicts
#' class probabilities, selects an optimal classification threshold, computes performance
#' metrics, generates ROC and Precision-Recall curves, produces confusion matrices, and
#' creates per-species performance maps.
#'
#' @param rf_model A trained Random Forest model object (from the \code{ranger} package).
#' @param test_data A data frame containing test features and labels. Must include a column
#'   named \code{Outliers} with factor levels \code{"outlier"} and \code{"valid"}.
#' @param tree_vote_optional_thresholds Numeric vector of thresholds to evaluate for converting
#'   predicted probabilities into class labels.
#' @param random_forest_results_folder Path to a folder where all outputs (plots, CSVs, maps)
#'   will be saved.
#'
#' @details
#' The function performs the following steps:
#' 1. Predicts class probabilities on the test dataset using the trained Random Forest.
#' 2. Extracts probabilities for the "outlier" class and runs \code{threshold_analysis_from_probs()} 
#'    to select the optimal classification threshold.
#' 3. Computes and plots Precision-Recall and ROC curves using \code{precrec} and \code{pROC}, 
#'    saving the plots as PNG files.
#' 4. Assigns predicted labels based on the chosen threshold.
#' 5. Computes a confusion matrix using \code{caret::confusionMatrix} and saves the raw table 
#'    and summary metrics as CSV files.
#' 6. Generates per-species and per-TAG performance maps using \code{performance_mapping_pipeline()}.
#'
#' @return
#' The function does not return an R object. All results are saved in the specified 
#' \code{random_forest_results_folder}, including:
#' \itemize{
#'   \item Precision-Recall curve PNG
#'   \item ROC curve PNG
#'   \item Confusion matrix CSV
#'   \item Classification metrics CSV
#'   \item Per-species and per-TAG performance maps
#' }
#'
#' @examples
#' \dontrun{
#' analyze_model_performance(
#'   rf_model = trained_rf_model,
#'   test_data = test_dataset,
#'   tree_vote_optional_thresholds = seq(0.1, 0.9, by = 0.05),
#'   random_forest_results_folder = "results/"
#' )
#' }
#'
#' @import ranger
#' @import ggplot2
#' @import precrec
#' @import pROC
#' @import caret
#' @import dplyr
#' @export
analyze_model_performance <- function(rf_model, 
                                      test_data,
                                      non_feature_column_names,
                                      tree_vote_optional_thresholds,
                                      random_forest_results_folder) {
  
  # To make the results reproducible across runs
  set.seed(42)
  
  # Remove the non-feature columns from the training set
  features_only <- test_data %>%
    dplyr::select(-all_of(non_feature_column_names))
  
  
  # Predict class probabilities
  pred_probs <- predict(rf_model, data = features_only, type = "response")$predictions
  
  # If label has two levels, extract probability for the "positive" class ("outlier")
  positive_class <- levels(features_only$Outliers)[1]
  probs_positive <- pred_probs[, positive_class]
  
  threshold <- threshold_analysis_from_probs(probs_positive = probs_positive,
                                             true_labels_vector = features_only$Outliers,
                                             thresholds = tree_vote_optional_thresholds,
                                             random_forest_results_folder = random_forest_results_folder)
  
  print(paste0("Best threshold: ", threshold))
  
  # Create a precrec object to evaluate both PR and ROC curves
  eval <- evalmod(scores = probs_positive, labels = features_only$Outliers)
  
  # Plot and show the curves
  autoplot(eval)
  
  # Save the plot as PNG
  ggsave(file.path(random_forest_results_folder, "Precision-Recall_curve.png"), width = 8, height = 6, dpi = 300)
  
  # Compute and plot ROC manually with pROC for extra control
  roc_obj <- roc(response = features_only$Outliers,
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
  
  # If the probability for "outlier" class is larger than the threshold, set the class as "outlier"
  pred_classes <- ifelse(pred_probs[, "outlier"] > threshold, "outlier", "valid")
  
  # Convert to factor
  pred_classes <- factor(pred_classes, levels = c("outlier", "valid"))
  
  # Add predicted labels as factor to the test
  test_data$predicted_outlier <- factor(pred_classes, levels = c("outlier", "valid"))
  
  # Evaluate the confusion matrix of the predictions
  conf_mat <- confusionMatrix(pred_classes, features_only$Outliers, positive = "outlier")
  print(conf_mat)
  
  # Get the confusion table without all the metrics
  conf_table <- as.data.frame(conf_mat$table)
  
  # Save the confusion table as csv
  write.csv(conf_table, file = file.path(random_forest_results_folder, "confusion_matrix.csv"), row.names = FALSE)
  
  ### Generate a table that summarizes the performance metrics of the model ###
  
  # Get confusion matrix metrics
  overall_metrics <- as.list(conf_mat$overall)
  by_class_metrics <- as.list(conf_mat$byClass)
  
  # Prefix column names to prevent collisions (optional, you can skip this step)
  # names(overall_metrics) <- paste0("Overall_", names(overall_metrics))
  # names(by_class_metrics) <- paste0("ByClass_", names(by_class_metrics))
  
  # Combine metrics into a single list and convert to one-row data frame
  combined_metrics <- c(overall_metrics, by_class_metrics)
  metrics_df <- as.data.frame(combined_metrics)
  
  # Save to CSV
  write.csv(metrics_df, file = file.path(random_forest_results_folder, "classification_metrics.csv"), row.names = FALSE)
  
  # Add predicted labels to test_data
  test_data$predicted_outlier <- factor(pred_classes, levels = c("outlier", "valid"))
  
  # Generate perormance maps per tag
  performance_mapping_pipeline(data_with_label_predictions = test_data,
                               reference_label_col = "Outliers",
                               predicted_label_col = "predicted_outlier",
                               output_dir = file.path(random_forest_results_folder, "Performance_maps"))
  
}