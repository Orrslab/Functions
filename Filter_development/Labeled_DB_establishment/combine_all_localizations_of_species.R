source(file.path(getwd(), "load_atlas_data_from_multiple_sqlite_files.R"))

#' Combine All Localizations for a Species
#'
#' This function loads localization and detection data from all SQLite files
#' within a species folder, combines them into a single dataset, and converts
#' the localization timestamps into human-readable datetime format.
#'
#' @param path_to_species Character. Path to the folder containing the SQLite files
#'   with labeled data for the species.
#'
#' @return A list containing at least the following components:
#'   \itemize{
#'     \item \code{LOCALIZATIONS} — Dataframe of combined localizations with an
#'       additional column \code{dateTime} (POSIXct) derived from \code{TIME}.
#'     \item \code{DETECTIONS} — Dataframe of combined detections (if present in files).
#'   }
#'
#' @details
#' This function identifies all \code{.sqlite} files in the species folder,
#' calls \code{\link{load_atlas_data_from_multiple_sqlite_files}} to load the data,
#' and appends a \code{dateTime} column to the \code{LOCALIZATIONS} table.
#'
#' @seealso \code{\link{load_atlas_data_from_multiple_sqlite_files}} for the
#'   lower-level function that performs the SQLite file reading.
#'
#' @export
combine_all_localizations_of_species <- function(path_to_species) {
  
  # Get all SQLite files in the folder
  sqlite_files_in_species_folder <- list.files(path_to_species, pattern = "\\.sqlite$", full.names = TRUE)
  
  # Open all the sqlite files and unite all the data in one R dataframe
  # combined_data <- load_localization_data_from_all_sqlite_files_in_folder(path_to_species)
  combined_data <- load_atlas_data_from_multiple_sqlite_files(sqlite_files_in_species_folder)
  
  # Convert `TIME` from milliseconds to human-readable datetime
  combined_data$LOCALIZATIONS$dateTime <- as.POSIXct(combined_data$LOCALIZATIONS$TIME / 1000, origin = "1970-01-01", tz = "UTC")
  
  return(combined_data)
  
}

