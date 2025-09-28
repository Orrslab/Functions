library(pROC)
library(data.table)

calculate_auc_per_feature <- function(df) {
  # Ensure it's a data.table
  df <- as.data.table(df)
  
  # Convert Outliers to numeric binary: 1 = outlier, 0 = valid
  df[, Outlier_numeric := ifelse(Outliers == "outlier", 1, 0)]
  
  # Identify feature columns (exclude Outliers and Species_id)
  feature_cols <- setdiff(names(df), c("Outliers", "Outlier_numeric", "Species_id"))
  
  # Initialize result list
  auc_results <- list()
  
  # Loop through features
  for (feature in feature_cols) {
    
    print(feature)
    
    # Check if feature has at least 2 unique values
    if (length(unique(df[[feature]])) > 1) {
      roc_obj <- tryCatch(
        roc(df$Outlier_numeric, df[[feature]], quiet = TRUE),
        error = function(e) NULL
      )
      auc_value <- if (!is.null(roc_obj)) as.numeric(auc(roc_obj)) else NA
    } else {
      auc_value <- NA
    }
    
    auc_results[[length(auc_results) + 1]] <- list(
      Feature = feature,
      AUC = auc_value
    )
  }
  
  # Return as a data.table
  return(rbindlist(auc_results))
}