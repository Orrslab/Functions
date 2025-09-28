library(pROC)

evaluate_roc_parameters <- function(feature_data, outliers_data){
  
  path_to_roc_analysis <- paste0(getwd(), "Build_atlasRF/ROC_analysis/")
  
  # Remove NA values from feature_data and Outliers before calculating quantiles
  feature_data_clean <- na.omit(feature_data)
  
  # Ensure feature_data_clean is not empty
  if (length(feature_data_clean) > 0) {
    # Find the indices of rows that are present in both location_data_labeled and feature_data_clean
    matched_indices <- which(!is.na(feature_data) & feature_data %in% feature_data_clean)
    
    # Subset location_data_labeled based on matched indices
    outliers_data_clean <- outliers_data[matched_indices]
  } else {
    stop("feature_data_clean is empty or contains no valid data.")
  }
  
  roc_curve <- roc(response = outliers_data_clean,
                   predictor = feature_data_clean,
                   auc = TRUE)
  
  # Extract TPR (Sensitivity) and FPR
  tpr <- roc_curve$sensitivities
  fpr <- 1 - roc_curve$specificities
  thresholds <- roc_curve$thresholds

  # Find the best threshold for classification with each feature
  source(paste0(path_to_roc_analysis, "roc_find_best_threshold.R"))
  best_thresholds <- roc_find_best_threshold(tpr, fpr, thresholds)
  
  # # Create a vector of Percentile-based thresholds
  # thresholds <- quantile(feature_data_clean, probs = seq(0, 1, length.out = 20))
  # 
  # # Compute the true positive rates (TPR) and FPR (False Positive Rate)
  # source(paste0(path_to_roc_analysis, "roc_analysis_calculate_tpr_and_fpr.R"))
  # feature_parameters <- roc_analysis_calculate_tpr_and_fpr(feature_data_clean, outliers_data_clean, thresholds)
  # 
  # # Calculate AUC
  # auc_value <- roc(response = outliers_data_clean, 
  #                  predictor = feature_data_clean, 
  #                  auc = TRUE)
  # 
  # # Find the best threshold for classification with each feature
  # source(paste0(path_to_roc_analysis, "roc_find_best_threshold.R"))
  # best_thresholds <- roc_find_best_threshold(feature_data = feature_data_clean, 
  #                                            outliers = outliers_data_clean)
  # 
  # return(list(tpr = feature_parameters$tpr, 
  #             fpr = feature_parameters$fpr, 
  #             auc = auc_value,
  #             best_threshold = best_thresholds))
  
  return(list(tpr = tpr, 
              fpr = fpr, 
              auc = as.numeric(roc_curve["auc"]),
              best_threshold = best_thresholds))
}

