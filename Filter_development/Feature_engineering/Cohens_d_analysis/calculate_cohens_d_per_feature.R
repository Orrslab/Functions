
library(data.table)
library(effsize)  # for cohen.d()

calculate_cohens_d_per_feature <- function(df) {
  
  # Ensure it's a data.table
  df <- as.data.table(df)
  
  # Ensure Outliers is a factor with 2 levels
  df[, Outliers := factor(Outliers)]
  
  # Identify numeric feature columns (exclude Outliers and non-numeric cols)
  feature_cols <- names(df)[sapply(df, is.numeric) & !names(df) %in% c("Outlier_numeric")]
  feature_cols <- setdiff(feature_cols, "Outliers")  # just to be safe
  
  # Result list
  cohens_d_results <- list()
  
  for (feature in feature_cols) {
    # Ensure the feature has both groups with data
    if (length(unique(df$Outliers)) == 2 && all(table(df$Outliers) > 1)) {
      d <- tryCatch({
        effsize::cohen.d(df[[feature]], df$Outliers)$estimate
      }, error = function(e) NA)
    } else {
      d <- NA
    }
    
    cohens_d_results[[length(cohens_d_results) + 1]] <- list(
      Feature = feature,
      Cohens_d = d
    )
  }
  
  # Return as data.table
  return(rbindlist(cohens_d_results))
}