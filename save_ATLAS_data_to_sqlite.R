# Load required packages
required_packages <- c("RSQLite", "DBI", "dplyr")
sapply(required_packages, library, character.only = TRUE)

source(file.path(getwd(), "Scripts", "config.R"))


#' Save ATLAS Data to SQLite Database
#'
#' This function saves localization and detection data to an SQLite database.
#' The database file is named based on the provided tag number, start time, 
#' and end time. Spaces in the time strings are replaced with underscores.
#'
#' @param localizations_data A data frame containing localization data. 
#'                           Default is NULL, meaning the data will not be saved if not provided.
#' @param detections_data A data frame containing detection data. 
#'                        Default is NULL, meaning the data will not be saved if not provided.
#' @param tag_number A unique identifier for the tag associated with the data.
#' @param start_time A character string representing the start time in 
#'                   the format 'YYYY-MM-DD HH:MM:SS'.
#' @param end_time A character string representing the end time in 
#'                 the format 'YYYY-MM-DD HH:MM:SS'.
#'
#' @return This function does not return any value. It saves the provided 
#'         data frames to the specified SQLite database file.
#'
#' @details The function connects to an SQLite database and writes the 
#'          localization and detection data to separate tables. If either 
#'          data frame is not provided, the corresponding table will not be created.
#'          The database connection is closed after the operation is complete.
#'          
save_ATLAS_data_to_sqlite <- function(localizations_data=NULL, detections_data=NULL, tag_number, start_time, end_time)
{
  # Replace spaces with underscores
  start_time_replace_spaces <- gsub(" ", "_", start_time)
  end_time_replace_spaces <- gsub(" ", "_", end_time)
  
  # Replace colons with minus signs
  start_time_replace_colons <- gsub(":", "-", start_time_replace_spaces)
  end_time_replace_colons <- gsub(":", "-", end_time_replace_spaces)
  
  # Create the .sqlite file name
  filename <- paste("db_", tag_number, "_", 
                    start_time_replace_colons, "_to_", 
                    end_time_replace_colons, ".sqlite", sep = "")
  filepath = paste0(path_to_sqlite_files, filename)
  print(filepath)
  
  # Connect to the SQLite database
  conn <- dbConnect(RSQLite::SQLite(), dbname = filepath)
  
  # Write localizations data to the database, if provided
  if (!is.null(localizations_data)) {
    dbWriteTable(conn, "LOCALIZATIONS", localizations_data, overwrite=TRUE)
    print("Localizations data saved.")
  }
  
  # Write detections data to the database, if provided
  if (!is.null(detections_data)) {
    dbWriteTable(conn, "DETECTIONS", detections_data, overwrite=TRUE)
    print("Detections data saved.")
  }
  
  # Close the database connection
  dbDisconnect(conn)
  
}
