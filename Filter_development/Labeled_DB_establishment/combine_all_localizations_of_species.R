source(file.path(getwd(), "load_atlas_data_from_multiple_sqlite_files.R"))

combine_all_localizations_of_species <- function(path_to_species) {
  
  # Get all SQLite files in the folder
  sqlite_files_in_species_folder <- list.files(path_to_species, pattern = "\\.sqlite$", full.names = TRUE)
  
  # Open all the sqlite files and unite all the data in one R dataframe
  # combined_data <- load_localizations_data_from_all_sqlite_files_in_folder(path_to_species)
  combined_data <- load_atlas_data_from_multiple_sqlite_files(sqlite_files_in_species_folder)
  
  # Convert `TIME` from milliseconds to human-readable datetime
  combined_data$LOCALIZATIONS$dateTime <- as.POSIXct(combined_data$LOCALIZATIONS$TIME / 1000, origin = "1970-01-01", tz = "UTC")
  
  return(combined_data)
  
}

