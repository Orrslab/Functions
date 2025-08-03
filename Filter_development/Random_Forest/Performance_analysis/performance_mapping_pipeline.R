
source(file.path(getwd(), "Filter_development/Random_Forest/Performance_analysis/add_confusion_categories_to_data.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/Performance_analysis/create_and_save_performance_maps_per_species_and_tag.R"))

performance_mapping_pipeline <- function(data_with_label_predictions) {
  
  message('Generating performance maps per species and tag:')
  
  # Check that required columns exist
  required_columns <- c("Outliers", "predicted_outlier")
  missing_columns <- setdiff(required_columns, names(data_with_label_predictions))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Add confusion tables categories to the data (True Positive, True Negative, False Positive and False Negative)
  data_with_confusion_table_categories <- add_confusion_categories_to_data(data_with_label_predictions)
  
  # Create performance maps per species and TAG
  create_and_save_performance_maps_per_species_and_tag(
    data = data_with_confusion_table_categories,
    color_column = "confusion_category",
    output_dir = file.path(random_forest_results_folder, "maps_by_species_and_tag")
  )
  
}

