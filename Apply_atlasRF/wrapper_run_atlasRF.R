
source(file.path(getwd(), "Apply_atlasRF/run_atlasRF_on_localization_data.R"))

wrapper_run_atlasRF <- function(cleaned_data, config) {
  
  message("*** Running atlasRF on the cleaned data. ***")
  
  # Load the atlasRF model of the analyzed species
  atlasRF_model <- readRDS(file.path(config$paths$folder_atlasRF_model, config$paths$filename_random_forest_model))
  
  # Run atlasRF
  localization_data_with_outliers_indication <- run_atlasRF_on_localization_data(
    cleaned_data, atlasRF_model, config$random_forest_settings$rf_prediction_threshold)
  
  
  return(localization_data_with_outliers_indication)
}