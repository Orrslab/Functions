
library(DBI)
library(RSQLite)

source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
source(file.path(getwd(), "save_ATLAS_data_to_sqlite.R"))

#' Combine labeled data of all species into a single SQLite database
#'
#' This function loads all labeled data from per-species SQLite files in the
#' specified combined species data folder and merges them into a single
#' SQLite database called `labeled_data_db.sqlite` saved in the root database path.
#'
#' @param path_to_db Character string.  
#'   Path to the root labeled data database folder where the combined SQLite
#'   database `labeled_data_db.sqlite` will be created.
#'
#' @param combined_species_data_folder Character string.  
#'   Path to the folder containing per-species `.sqlite` files (typically
#'   the `Combined_species_data` subfolder of `path_to_db`).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Lists all `.sqlite` files in \code{combined_species_data_folder}.
#'   \item Loads localization and detection data from each file using
#'         \code{load_atlas_data_from_sqlite()}.
#'   \item Extracts a \code{species_id} from the first two characters of each file name.
#'   \item Adds the \code{species_id} column to both localization and detection datasets.
#'   \item Combines all species' localizations into one dataframe and all detections
#'         into another.
#'   \item Saves the combined datasets into a single SQLite file called
#'         \code{labeled_data_db.sqlite} in \code{path_to_db}, using
#'         \code{save_ATLAS_data_to_sqlite()}.
#' }
#'
#' @return
#' This function is called for its side effects.  
#' It writes `labeled_data_db.sqlite` to `path_to_db` and prints confirmation
#' messages. No value is returned.
#'
#' @note
#' This function depends on:
#' \itemize{
#'   \item \code{load_atlas_data_from_sqlite()} — for reading individual species data.
#'   \item \code{save_ATLAS_data_to_sqlite()} — for writing the combined database.
#' }
#' Ensure both functions are available in the environment (e.g., via \code{source()}).
#'
#' @examples
#' \dontrun{
#' combine_all_species_data(
#'   path_to_db = "C:/path/to/Labeled_data_DB",
#'   combined_species_data_folder = "C:/path/to/Labeled_data_DB/Combined_species_data"
#' )
#' }
#'
#' @import DBI
#' @import RSQLite
#' @export
combine_all_species_data <- function(path_to_db,
                                     combined_species_data_folder) {
  
  message("Combining the labeled data of all species into one file.")
  
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
