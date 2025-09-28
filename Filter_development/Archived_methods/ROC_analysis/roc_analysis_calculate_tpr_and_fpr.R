roc_analysis_calculate_tpr_and_fpr <- function(feature_data, outliers_data, thresholds) {
  # Initialize vectors for storing TPR (True Positive Rate) and FPR (False Positive Rate)
  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))
  
  for (i in 1:length(thresholds)) {
    # Convert feature data to binary predictions based on the threshold
    predictions <- ifelse(feature_data >= thresholds[i], 1, 0)
    
    # Compute confusion matrix components
    tp <- sum(predictions == 1 & outliers_data == 1)  # True positives
    fp <- sum(predictions == 1 & outliers_data == 0)  # False positives
    tn <- sum(predictions == 0 & outliers_data == 0)  # True negatives
    fn <- sum(predictions == 0 & outliers_data == 1)  # False negatives
    
    # Compute TPR and FPR
    tpr[i] <- tp / (tp + fn)  # True Positive Rate (Sensitivity)
    fpr[i] <- fp / (fp + tn)  # False Positive Rate (1 - Specificity)
  }
  
  # Return TPR and FPR values for ROC curve plotting
  return(list(tpr = tpr, fpr = fpr))
}
