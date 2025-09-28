
clean_atlasRF_results <- function(localization_data_with_outlier_labels) {
  
  message("*** Cleaninig atlasRF data. ***")
  
  message("Removing rows with Outliers == NA")
  localization_data_with_outlier_labels <- localization_data_with_outlier_labels[!is.na(localization_data_with_outlier_labels$Outliers), ]
  
  # Convert Outliers column from factor to binary to use on the Visual Filter App
  # Also- change the column name from 'Outliers' to 'Outliers_atlasRF'
  localization_data_with_outlier_labels$Outliers_atlasRF <- ifelse(
    as.character(localization_data_with_outlier_labels$Outliers) == "outlier", 1, 0
  )
  
  # Remove the Outliers column
  localization_data_with_outlier_labels$Outliers <- NULL
  
  return(localization_data_with_outlier_labels)
  
}