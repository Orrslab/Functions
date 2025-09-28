library(caret)

evaluate_model_performance_vs_other_filter <- function(data,
                                                       reference_label_col,
                                                       predicted_label_col,
                                                       positive_class = "outlier",
                                                       output_dir = "model_evaluation_results") {
  # Check if columns exist
  if (!(reference_label_col %in% names(data))) {
    stop(paste("Column", reference_label_col, "not found in the data frame."))
  }
  if (!(predicted_label_col %in% names(data))) {
    stop(paste("Column", predicted_label_col, "not found in the data frame."))
  }

  # Extract the relevant columns
  reference <- data[[reference_label_col]]
  predicted <- data[[predicted_label_col]]
  
  # Convert to factors with consistent levels
  levels_union <- union(levels(factor(reference)), levels(factor(predicted)))
  reference <- factor(reference, levels = levels_union)
  predicted <- factor(predicted, levels = levels_union)
  
  # Compute confusion matrix
  cm <- confusionMatrix(predicted, reference, positive = positive_class)
  
  # Save confusion matrix
  cm_df <- as.data.frame(cm$table)
  write.csv(cm_df, paste0(output_dir, "_confusion_matrix.csv"), row.names = FALSE)
  
  # Save metrics
  metrics_overall <- as.data.frame(t(cm$overall))
  metrics_byClass <- as.data.frame(t(cm$byClass))
  metrics_combined <- cbind(metrics_overall, metrics_byClass)
  write.csv(metrics_combined, paste0(output_dir, "_metrics.csv"), row.names = FALSE)
}