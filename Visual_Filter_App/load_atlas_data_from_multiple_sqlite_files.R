
#' Load and consolidate data from multiple SQLite files
#'
#' This function retrieves data from multiple SQLite files, consolidating it into 
#' a single data frame. It uses the `load_atlas_data_from_sqlite` function to load 
#' data from each file and combines the results.
#'
#' @param fullpaths_to_sqlite_files A character vector containing the full paths 
#'   to the SQLite files. Each path should point to a valid SQLite file containing 
#'   localization data. If a file is missing or unreadable, the function will stop
#'   execution with an error.
#'
#' @return A data frame containing the combined localization data from all the 
#'   specified SQLite files. If no valid files are provided, an empty data frame 
#'   is returned.
#'
#' @details 
#' This function iterates over the provided list of SQLite file paths, 
#' calls the `load_atlas_data_from_sqlite` function to load data from each file, 
#' and appends the data into a list. After all files are processed, the data frames 
#' are combined using `dplyr::bind_rows` to create a single consolidated data frame.
#'
#' **Assumptions:**
#' - The SQLite files must contain valid localization data.
#' - The `load_atlas_data_from_sqlite.R` script must be available in the working directory.
#' - All SQLite files should have a consistent schema; otherwise, `bind_rows()` may fail.
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
  
  # Load required packages
  required_packages <- c("RSQLite", "dplyr")
  sapply(required_packages, library, character.only = TRUE)
  
  # Initialize a list to store data from each file
  all_data_frames_loc <- list()  
  all_data_frames_det <- list()  
  
  for (sqlite_filepath in fullpaths_to_sqlite_files) {
    
    if (!file.exists(sqlite_filepath)) {
      warning(paste("Skipping missing file:", sqlite_filepath))
      next
    }
    
    source(paste0(getwd(), "/load_atlas_data_from_sqlite.R"))
    raw_atlas_data <- tryCatch(
      {
        load_atlas_data_from_sqlite(sqlite_filepath)
      },
      error = function(e) {
        warning(paste("Error loading file:", sqlite_filepath, "->", e$message))
        return(NULL)
      }
    )
    
    # Append the data from the current file to the list
    all_data_frames_loc[[length(all_data_frames_loc) + 1]] <- raw_atlas_data$LOCALIZATIONS
    if (!is.null(raw_atlas_data$DET)) {
      all_data_frames_det[[length(all_data_frames_det) + 1]] <- raw_atlas_data$DETECTIONS
    }
  }
  
  # Combine all data frames into one
  raw_location_data <- bind_rows(all_data_frames_loc)
  raw_detection_data <- if (length(all_data_frames_det) > 0) {
    bind_rows(all_data_frames_det)
  } else {
    NULL  # No detection data retrieved
  }
  
  # Return the consolidated data
  return(list(
    locations = raw_location_data,
    detections = raw_detection_data
  ))
}