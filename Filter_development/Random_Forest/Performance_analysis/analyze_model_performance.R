
source(file.path(getwd(), "Filter_development/Random_Forest/Performance_analysis/threshold_analysis.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/Performance_analysis/performance_mapping_pipeline.R"))

analyze_model_performance <- function(rf_model, test_tata) {
  
  # Predict class probabilities
  pred_probs <- predict(rf_model_final, data = features_only, type = "response")$predictions
  
  # If label has two levels, extract probability for the "positive" class ("outlier")
  positive_class <- levels(test_data$Outliers)[1]
  probs_positive <- pred_probs[, positive_class]
  
  threshold <- threshold_analysis_from_probs(probs_positive = probs_positive, 
                                             true_labels_vector = test_tata$Outliers,
                                             random_forest_results_folder = random_forest_results_folder)
  
  print(paste0("Best threshold: ", threshold))
  
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
  
  # If the probability for "outlier" class is larger than the threshold, set the class as "outlier"
  pred_classes <- ifelse(pred_probs[, "outlier"] > threshold, "outlier", "valid")
  
  # Convert to factor
  pred_classes <- factor(pred_classes, levels = c("outlier", "valid"))
  
  # Add predicted labels as factor to the test
  test_data$predicted_outlier <- factor(pred_classes, levels = c("outlier", "valid"))
  
  # Evaluate the confusion matrix of the predictions
  conf_mat <- confusionMatrix(pred_classes, test_data$Outliers, positive = "outlier")
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
  
  # Generate perormance maps per tag
  performance_mapping_pipeline(data_with_label_predictions = test_data)
  
}