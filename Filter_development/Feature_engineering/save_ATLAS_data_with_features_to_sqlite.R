
save_ATLAS_data_with_features_to_sqlite <- function(
    localizations_data = NULL, 
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
  
  # Save localizations_data if given
  if (!is.null(localizations_data)) {
    dbWriteTable(conn, "LOCALIZATIONS", localizations_data, overwrite = TRUE)
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