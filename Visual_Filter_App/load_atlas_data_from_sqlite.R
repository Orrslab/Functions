
#' Load ATLAS Localizations Data from an SQLite Database
#'
#' This function loads localization data from an SQLite database file that stores ATLAS tracking data.
#' The function retrieves all records from the "LOCALIZATIONS" table and returns them as a data frame.
#'
#' @param sqlite_filepath A character string representing the full path to the SQLite database file.
#'
#' @return A data frame containing the localization data from the "LOCALIZATIONS" table.
#'         If the file does not exist, the function returns `NULL`.
#'
#' @details 
#' This function connects to an SQLite database, queries all available data from the "LOCALIZATIONS" table, 
#' and then disconnects from the database. The function does **not** retrieve data from the "DETECTIONS" table.
#'
#' The expected columns in the returned data frame include (but are not limited to):
#' - `TAG` (integer): The tag identifier.
#' - `TIME` (numeric): Timestamp of the localization.
#' - `X`, `Y`, `Z` (numeric): Spatial coordinates of the localization.
#' - Additional variables describing localization accuracy and constraints.
#'
#' If the specified SQLite file does not exist, a warning is issued, and the function returns `NULL`.
#'
#' @examples
#' # Define the file path
#' file_name <- "BO_0556_from_2021-07-04_17-00-03_to_2021-07-04_23-59-58_filtered.sqlite"
#' path <- "C:/data/Filtered_data/"
#' file_path <- paste0(path, file_name)
#'
#' # Load the data
#' data <- load_atlas_data_from_sqlite(file_path)
#'
#' # Check the first few rows
#' if (!is.null(data)) head(data)
#'
load_atlas_data_from_sqlite <- function(sqlite_filepath) {
  
  # Load required packages
  required_packages <- c("RSQLite", "DBI")
  invisible(lapply(required_packages, library, character.only = TRUE))
  
  # check if the file exists
  if (!file.exists(sqlite_filepath)) {
    warning(paste0("Warning: Cannot load file from sqlite: \n",
                   "The file '", basename(sqlite_filepath), "' does not exist.\n"))
    return(NULL) # Exit the function
  }
  
  conn <- dbConnect(RSQLite::SQLite(), dbname=sqlite_filepath)
  
  RawLoc0 <- dbGetQuery(conn, "SELECT * FROM LOCALIZATIONS")
  RawDet0 <- dbGetQuery(conn, "SELECT * FROM DETECTIONS")
  
  dbDisconnect(conn)

  # Return the Localizations and Detections data
  return(list("DETECTIONS"=RawDet0,"LOCALIZATIONS"=RawLoc0))
}