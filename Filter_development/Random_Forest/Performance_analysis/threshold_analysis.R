library(caret)
library(ggplot2)
library(reshape2)

threshold_analysis_from_probs <- function(probs_positive, true_labels_vector,
                                          thresholds = seq(0.1, 0.9, by = 0.05),
                                          random_forest_results_folder,
                                          output_csv = "threshold_analysis_results.csv",
                                          output_plot = "threshold_analysis_plot.png") {
  
  # Ensure labels are factor with correct levels
  true_labels_vector <- factor(true_labels_vector)
  
  results <- data.frame()
  
  for (thresh in thresholds) {
    pred_labels <- factor(ifelse(probs_positive >= thresh, "outlier", "valid"), levels = c("outlier", "valid"))
    
    cm <- confusionMatrix(pred_labels, true_labels_vector, positive = "outlier")
    
    results <- rbind(results, data.frame(
      Threshold = thresh,
      Accuracy = cm$overall["Accuracy"],
      Precision = cm$byClass["Precision"],
      Recall = cm$byClass["Sensitivity"],
      F1 = cm$byClass["F1"]
    ))
  }
  
  # Save results
  write.csv(results, file.path(random_forest_results_folder, output_csv), row.names = FALSE)
  
  # Plot
  results_long <- melt(results, id.vars = "Threshold")
  
  p <- ggplot(results_long, aes(x = Threshold, y = value, color = variable)) +
    geom_line(size = 1.2) +
    geom_point() +
    labs(title = "Threshold Analysis", y = "Metric Value", x = "Threshold") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  ggsave(file.path(random_forest_results_folder, output_plot), plot = p, width = 8, height = 5, dpi = 300, bg = "white")
  
  # Get the best threshold by F1
  best_row <- results[which.max(results$F1), ]
  best_threshold <- best_row$Threshold
  
  return(best_threshold)

}