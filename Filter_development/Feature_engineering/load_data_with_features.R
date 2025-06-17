# Load the data frame with the raw ATLAS data + feature values + Outliers labels

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))

load_data_with_features <- function(tables_to_load = "LOCALIZATIONS") {
  
  # USER INPUT BEGIN
  path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
  features_data_filename <- "Features_data_for_RF_all_species.sqlite"
  # USER'S INPUT END
  
  features_filepath <- file.path(path_to_data_with_features, features_data_filename)
  
  # Load the features data from sqlite
  features_data <- load_tables_from_sqlite_file(
    sqlite_filepath = features_filepath,
    tables = tables_to_load)
  
  
  return(features_data)
  
}

