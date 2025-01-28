library(pROC)

# Function to determine the best threshold
roc_find_best_threshold <- function(tpr, fpr, thresholds) {
  # # Compute ROC
  # roc_curve <- roc(response = outliers, predictor = feature_data)
  # 
  # # Extract TPR (Sensitivity) and FPR
  # tpr <- roc_curve$sensitivities
  # fpr <- 1 - roc_curve$specificities
  # thresholds <- roc_curve$thresholds
  
  # Calculate Youden's J Statistic
  youden_j <- tpr - fpr
  
  # Find the optimal threshold based on max Youden's J
  best_index <- which.max(youden_j)
  best_threshold <- thresholds[best_index]
  
  # Alternative: Find the closest to (0,1) on ROC curve
  distance <- sqrt((1 - tpr)^2 + fpr^2)
  best_index_dist <- which.min(distance)
  best_threshold_dist <- thresholds[best_index_dist]
  
  # Return both options
  return(list(best_threshold_youden = best_threshold, 
              best_threshold_distance = best_threshold_dist))
}

# # Example usage
# best_thresholds <- find_best_threshold(speed_roc_parameters$fpr, location_data_labeled$Outliers)
# print(best_thresholds)