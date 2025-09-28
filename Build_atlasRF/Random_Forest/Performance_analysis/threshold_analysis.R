library(caret)
library(ggplot2)
library(reshape2)

#' Threshold Analysis for Binary Classification Probabilities
#'
#' This function evaluates multiple probability thresholds for converting predicted
#' probabilities into class labels in a binary classification problem. For each threshold,
#' it calculates common performance metrics (Accuracy, Precision, Recall, F1), saves
#' the results to a CSV file, and produces a plot showing metric values across thresholds.
#' The threshold that maximizes F1 score is returned.
#'
#' @param probs_positive Numeric vector of predicted probabilities for the positive class
#'   (e.g., "outlier").
#' @param true_labels_vector Factor vector of true class labels with levels "outlier" and "valid".
#' @param thresholds Numeric vector of probability thresholds to evaluate. Default is 
#'   \code{seq(0.1, 0.9, by = 0.05)}.
#' @param random_forest_results_folder Path to a folder where CSV results and the plot
#'   will be saved.
#' @param output_csv Filename for the CSV file storing threshold metrics. Default is 
#'   \code{"threshold_analysis_results.csv"}.
#' @param output_plot Filename for the plot showing metrics vs. thresholds. Default is
#'   \code{"threshold_analysis_plot.png"}.
#'
#' @details
#' The function performs the following steps:
#' 1. Converts \code{true_labels_vector} to a factor.
#' 2. For each threshold in \code{thresholds}, assigns class labels based on whether the
#'    predicted probability is greater than or equal to the threshold.
#' 3. Computes a confusion matrix and extracts Accuracy, Precision, Recall, and F1 metrics.
#' 4. Aggregates results into a data frame and saves them as a CSV file.
#' 5. Creates a line plot showing the metrics as a function of threshold and saves it as PNG.
#' 6. Returns the threshold that maximizes the F1 score.
#'
#' @return
#' Numeric value of the best threshold that maximizes the F1 score.
#'
#' @examples
#' \dontrun{
#' best_thresh <- threshold_analysis_from_probs(
#'   probs_positive = predicted_probs$outlier,
#'   true_labels_vector = test_data$Outliers,
#'   thresholds = seq(0.1, 0.9, by = 0.05),
#'   random_forest_results_folder = "results/"
#' )
#' }
#'
#' @import caret
#' @import ggplot2
#' @import reshape2
#' @export
threshold_analysis_from_probs <- function(probs_positive, 
                                          true_labels_vector,
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