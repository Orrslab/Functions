
# Load required packages
required_packages <- c("RSQLite", "dplyr")
sapply(required_packages, library, character.only = TRUE)

source(file.path(getwd(), "config.R"))

#' Load and consolidate data from multiple SQLite files
#'
#' This function retrieves data from multiple SQLite files, consolidating it into 
#' a single data frame. It uses the `load_atlas_data_from_sqlite` function to load 
#' data from each file and combines the results.
#'
#' @param fullpaths_to_sqlite_files A character vector containing the full paths 
#'   to the SQLite files. Each path should point to a valid SQLite file containing 
#'   localization data.
#'
#' @return A data frame containing the combined localization data from all the 
#'   specified SQLite files.
#'
#' @details This function iterates over the provided list of SQLite file paths, 
#'   calls the `load_atlas_data_from_sqlite` function to load data from each file, 
#'   and appends the data into a list. After all files are processed, the data frames 
#'   are combined using `dplyr::bind_rows` to create a single consolidated data frame.
#'
#' @examples
#' # Example: Load data from multiple SQLite files
#' fullpaths_to_sqlite_files <- c(
#'   "path/to/sqlite_file1.sqlite",
#'   "path/to/sqlite_file2.sqlite"
#' )
#' combined_data <- load_atlas_data_from_multiple_sqlite_files(fullpaths_to_sqlite_files)
#'
#' @seealso \code{\link{load_atlas_data_from_sqlite}} for loading data from a single SQLite file.
#'
#' @import RSQLite
#' @import dplyr
#' 
load_atlas_data_from_multiple_sqlite_files <- function(fullpaths_to_sqlite_files) {
  # Initialize a list to store data from each file
  all_data_frames <- list()  
  
  for (sqlite_filepath in fullpaths_to_sqlite_files) {
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