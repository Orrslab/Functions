
# Load required packages
required_packages <- c("RSQLite", "dplyr")
sapply(required_packages, library, character.only = TRUE)

source(file.path(getwd(), "config.R"))

#' Load data from multiple SQLite files and consolidate into a single data frame.
#'
#' This function takes a list of SQLite file paths, calls the 
#' `load_atlas_from_sqlite` function for each file to retrieve the localization 
#' data, and combines all the data into one data frame.
#'
#' @param sqlite_filepaths A character vector containing the paths to the SQLite files.
#'
#' @return A data frame containing the consolidated localization data from all specified SQLite files.
#'
load_atlas_data_from_multiple_sqlite_files <- function(sqlite_filepaths) {
  # Initialize a list to store data from each file
  all_data_frames <- list()  
  
  for (sqlite_filepath in sqlite_filepaths) {
    source(paste0(path_to_atlas_data_analysis_repo, "load_atlas_data_from_sqlite.R"))
    RawLoc0 <- load_atlas_data_from_sqlite(sqlite_filepath)
    
    # Append the data from the current file to the list
    all_data_frames[[length(all_data_frames) + 1]] <- RawLoc0
  }
  
  # Combine all data frames into one
  raw_location_data <- bind_rows(all_data_frames)
  
  # Return the consolidated data
  return(raw_location_data)
}