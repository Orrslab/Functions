#' Save ATLAS Data and Features to SQLite Database
#'
#' Saves one or more ATLAS-related datasets to an SQLite database file using the \code{RSQLite} and \code{DBI} packages.
#'
#' @param localization_data A `data.frame` or `data.table` of localization data (optional).
#' @param detections_data A `data.frame` or `data.table` of detection data (optional).
#' @param participating_base_stations A `data.frame` of base stations that participated in detections (optional).
#' @param missed_base_stations A `data.frame` of base stations that were expected but did not participate in the localization (optional).
#' @param fullpath A character string specifying the full path to the SQLite database file. Default is the working directory.
#'
#' @return No return value. Writes data tables directly to the SQLite database file.
#'
#' @details
#' This function saves the provided data tables into a SQLite database with predefined table names:
#' \itemize{
#'   \item `"LOCALIZATIONS"` for the localization data,
#'   \item `"DETECTIONS"` for the detection data,
#'   \item `"PARTICIPATING_BASE_STATIONS"` for the participating base stations in each localization,
#'   \item `"MISSED_BASE_STATIONS"` for the missed base stations in each localization,
#'   \item `"PROPERTIES"` for general metadata (currently hard-coded with key `"atlas-system"` and value `"harod"`).
#' }
#'
#' Existing tables with the same names will be overwritten.
#'
#' @import RSQLite
#' @import DBI
#' @import dplyr
#' @import crayon
#'
#' @examples
#' \dontrun{
#' save_ATLAS_data_with_features_to_sqlite(
#'   localization_data = locs_df,
#'   detections_data = det_df,
#'   fullpath = "path/to/database.sqlite"
#' )
#' }
#'
#' @export

save_ATLAS_data_with_features_to_sqlite <- function(
    localization_data = NULL, 
    detections_data = NULL, 
    participating_base_stations = NULL,
    missed_base_stations = NULL,
    fullpath = getwd()
) {
  # Load required packages
  required_packages <- c("RSQLite", "DBI", "dplyr", "crayon")
  invisible(lapply(required_packages, library, character.only = TRUE))
  
  fullpath <- as.character(fullpath)
  
  # Connect to the SQLite database
  conn <- dbConnect(RSQLite::SQLite(), dbname = fullpath)
  
  # Save localization_data if given
  if (!is.null(localization_data)) {
    dbWriteTable(conn, "LOCALIZATIONS", localization_data, overwrite = TRUE)
    message("Localizations data saved.")
  }
  
  # Save detections_data if given
  if (!is.null(detections_data)) {
    dbWriteTable(conn, "DETECTIONS", detections_data, overwrite = TRUE)
    message("Detections data saved.")
  }
  
  # Save participating_base_stations if given
  if (!is.null(participating_base_stations)) {
    dbWriteTable(conn, "PARTICIPATING_BASE_STATIONS", participating_base_stations, overwrite = TRUE)
    message("Participating base stations data saved.")
  }
  
  # Save missed_base_stations if given
  if (!is.null(missed_base_stations)) {
    dbWriteTable(conn, "MISSED_BASE_STATIONS", missed_base_stations, overwrite = TRUE)
    message("Missed base stations data saved.")
  }
  
  # Create and insert data into the PROPERTIES table
  properties_data <- data.frame(KEY = "atlas-system", VALUE = "harod")
  dbWriteTable(conn, "PROPERTIES", properties_data, overwrite = TRUE)
  
  # Close the connection
  dbDisconnect(conn)
}