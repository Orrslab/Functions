
library(data.table)
library(effsize)  # for cohen.d()

calculate_cohens_d_per_feature_per_species <- function(df) {
  # Ensure input is a data.table
  df <- as.data.table(df)
  
  # Make sure Outliers is a factor
  df[, Outliers := factor(Outliers)]
  
  # Identify numeric feature columns (exclude Outliers and other non-numeric)
  feature_cols <- names(df)[sapply(df, is.numeric)]
  feature_cols <- setdiff(feature_cols, c("Outlier_numeric"))  # exclude this helper column if present
  
  # Initialize results list
  cohens_d_results <- list()
  
  for (species in unique(df$Species_id)) {
    df_species <- df[Species_id == species]
    
    # Only if both outlier classes are present
    if (length(unique(df_species$Outliers)) == 2 && all(table(df_species$Outliers) > 1)) {
      for (feature in feature_cols) {
        d <- tryCatch({
          effsize::cohen.d(df_species[[feature]], df_species$Outliers)$estimate
        }, error = function(e) NA)
        
        cohens_d_results[[length(cohens_d_results) + 1]] <- list(
          Species_id = species,
          Feature = feature,
          Cohens_d = d
        )
      }
    } else {
      for (feature in feature_cols) {
        cohens_d_results[[length(cohens_d_results) + 1]] <- list(
          Species_id = species,
          Feature = feature,
          Cohens_d = NA
        )
      }
    }
  }
  
  # Combine into data.table
  return(rbindlist(cohens_d_results))
}