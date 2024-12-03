
# Load required packages
required_packages <- c("RSQLite", "DBI")
sapply(required_packages, library, character.only = TRUE)

source(file.path(getwd(), "config.R"))

#' Load ATLAS Localizations Data from SQLite Database
#'
#' This function loads the localization data from an SQLite database file
#' that contains tables for ATLAS tracking data. The data from the 
#' "LOCALIZATIONS" table is retrieved and returned as a data frame.
#'
#' @param sqlite_filepath A character string representing the path to the SQLite database file.
#'
#' @return A data frame containing the data from the "LOCALIZATIONS" table in the SQLite database.
#'
#' @details This function connects to an SQLite database, retrieves all the data
#' from the "LOCALIZATIONS" table, and then disconnects from the database. The 
#' function returns the data as a data frame. The "DETECTIONS" table is currently
#' not retrieved (commented out for potential future use).
#' 
load_atlas_data_from_sqlite <- function(sqlite_filepath) {
  
  # check if the file exists
  if (!file.exists(sqlite_filepath)) {
    warning(paste0("Warning: Cannot load file from sqlite: \n",
                   "The file '", basename(sqlite_filepath), "' does not exist.\n"))
    return(NULL) # Exit the function
  }
  
  conn <- dbConnect(RSQLite::SQLite(), dbname=sqlite_filepath)
  
  RawLoc0 <- dbGetQuery(conn, "SELECT * FROM LOCALIZATIONS")
  
  dbDisconnect(conn)

  # Return the Localizations data
  return(RawLoc0)
}

# Usage example
# file_name <- "eminem_night17.sqlite"
# path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Raw_data/"
# file_path <- paste0(path, file_name)
# data <- load_atlas_data_from_sqlite(file_path)