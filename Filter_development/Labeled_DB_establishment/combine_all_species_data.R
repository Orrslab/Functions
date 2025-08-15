
library(DBI)
library(RSQLite)

source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
source(file.path(getwd(), "save_ATLAS_data_to_sqlite.R"))

#' Combine labeled data of all species into a single SQLite database
#'
#' This function loads all labeled data from per-species SQLite files in the
#' `Combined_species_data` subfolder of the specified database path, and
#' combines them into one SQLite file called `labeled_data_db.sqlite`.
#'
#' @param path_to_db Character string.  
#'   Path to the root labeled data database folder. This folder must contain a
#'   subfolder called `Combined_species_data` with per-species `.sqlite` files.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Lists all `.sqlite` files in `Combined_species_data`.
#'   \item Loads localization and detection data from each file using
#'         \code{load_atlas_data_from_sqlite()}.
#'   \item Adds a `species_id` column (first two characters of the file name)
#'         to each dataset.
#'   \item Combines all localizations into one dataframe and all detections
#'         into another.
#'   \item Saves the combined data into `labeled_data_db.sqlite` using
#'         \code{save_ATLAS_data_to_sqlite()}.
#' }
#'
#' @return
#' This function is called for its side effects. It writes
#' `labeled_data_db.sqlite` to `path_to_db` and prints a confirmation message.
#' No value is returned.
#'
#' @note
#' This function depends on:
#' \itemize{
#'   \item \code{load_atlas_data_from_sqlite()} — for reading data.
#'   \item \code{save_ATLAS_data_to_sqlite()} — for writing combined data.
#' }
#' Both must be available in the environment (via \code{source()} or a package).
#'
#' @examples
#' \dontrun{
#' combine_all_species_data("C:/path/to/Labeled_data_DB")
#' }
#'
#' @import DBI
#' @import RSQLite
#' @export
combine_all_species_data <- function(path_to_db) {
  
  combined_species_data_folder <- file.path(path_to_db, "Combined_species_data")
  
  # Open all the sqlite files in the combined species folder
  # List all sqlite files in the folder
  sqlite_files <- list.files(combined_species_data_folder, pattern = "\\.sqlite$", full.names = TRUE)
  
  # Initialize empty data frames before the loop
  all_data_localizations <- data.frame()
  all_data_detections <- data.frame()
  
  for (sqlite_file in sqlite_files) {
    
    # Get just the file name without the path
    file_name <- basename(sqlite_file)
    
    # Load the localizations and detections from the sqlite file
    data <- load_atlas_data_from_sqlite(sqlite_file)
    
    # Get the localizations and detections data
    localization_data <- data$LOCALIZATIONS
    detection_data <- data$DETECTIONS
    
    # Extract the first two letters for species_id
    species_id <- substr(file_name, 1, 2)
    
    # Add the species_id as a column in the beginning of the data table
    localization_data <- cbind(species_id = species_id, localization_data)
    detection_data <- cbind(species_id = species_id, detection_data)
    
    # Append the localizations and detections to the big data frames
    all_data_localizations <- rbind(all_data_localizations, localization_data)
    all_data_detections <- rbind(all_data_detections, detection_data)
    
  }
  
  save_ATLAS_data_to_sqlite(localization_data = all_data_localizations,
                            detections_data = all_data_detections,
                            fullpath = file.path(path_to_db, "labeled_data_db.sqlite"))
  
  message("Combined the labeled data of all species into one sqlite file.")
  
}
