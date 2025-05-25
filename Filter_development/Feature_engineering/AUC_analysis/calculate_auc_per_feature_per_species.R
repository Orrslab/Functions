library(pROC)
library(data.table)
library(tidyr)

calculate_auc_per_feature_per_species <- function(df) {
  # Ensure it's a data.table
  df <- as.data.table(df)
  
  # Make sure Outliers is binary numeric (1 = outlier, 0 = valid)
  df[, Outlier_numeric := ifelse(Outliers == "outlier", 1, 0)]
  
  # Identify feature columns (exclude Outliers and Species_id)
  feature_cols <- setdiff(names(df), c("Outliers", "Species_id", "Outlier_numeric"))
  
  # Result list
  auc_results <- list()
  
  # Loop through each species and each feature
  for (species in unique(df$Species_id)) {
    df_species <- df[Species_id == species]
    
    print(species)
    
    for (feature in feature_cols) {
      
      # Check if the feature has at least 2 unique values
      if (length(unique(df_species[[feature]])) > 1) {
        roc_obj <- tryCatch(
          roc(df_species$Outlier_numeric, df_species[[feature]], quiet = TRUE),
          error = function(e) NULL
        )
        auc_value <- if (!is.null(roc_obj)) as.numeric(auc(roc_obj)) else NA
      } else {
        auc_value <- NA
      }
      
      auc_results[[length(auc_results) + 1]] <- data.table(
        Feature = feature,
        Species_id = species,
        AUC = auc_value
      )
    }
  }
  
  # Combine the list of data.tables into one data.table
  return(rbindlist(auc_results))
}