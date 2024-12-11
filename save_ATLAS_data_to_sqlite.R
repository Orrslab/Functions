# Load required packages
required_packages <- c("RSQLite", "DBI", "dplyr", "crayon")
sapply(required_packages, library, character.only = TRUE)

source(file.path(getwd(), "config.R"))

#' Save ATLAS Data to SQLite Database
#'
#' This function saves localization and detection data to an SQLite database. 
#' The database file is named based on the provided tag number(s), start time, 
#' and end time.
#'
#' @param localizations_data A data frame containing localization data. 
#'                           Default is NULL, meaning the data will not be saved if not provided.
#' @param detections_data A data frame containing detection data. 
#'                        Default is NULL, meaning the data will not be saved if not provided.
#' @param tag_numbers A unique identifier (or identifiers) for the tag(s) 
#'                    associated with the data. Used to generate the SQLite 
#'                    file name.
#' @param start_time A character string representing the start time in the 
#'                   format 'YYYY-MM-DD HH:MM:SS'. Used to generate the 
#'                   SQLite file name.
#' @param end_time A character string representing the end time in the 
#'                 format 'YYYY-MM-DD HH:MM:SS'. Used to generate the 
#'                 SQLite file name.
#'
#' @param fullpath the path in which the sqlite file should be saved
#'
#' @return This function does not return any value. It saves the provided 
#'         data frames to the specified SQLite database file.
#'
#' @details The function connects to an SQLite database and writes the 
#'          localization and detection data to separate tables. If either 
#'          data frame is not provided, the corresponding table will not be created.
#'          The database connection is closed after the operation is complete.
#'          
save_ATLAS_data_to_sqlite <- function(localizations_data=NULL, detections_data=NULL, fullpath)
{
  # If a file with the same name exists, ask the user whether to overwrite it
  fullpath <- as.character(fullpath)
  if (file.exists(fullpath)) {
    repeat {
      cat(green("The file: \n'", basename(fullpath), 
                "' already exists.\n", "Do you want to overwrite it? (y/n): \n"))
      user_input <- readline()
      
      if (tolower(user_input) == "y") {
        # If 'y', proceed with overwriting
        break
      } else if (tolower(user_input) == "n") {
        # If 'n', stop the function or print a message
        message("Data will not be saved into sqlite.")
        return()
      } else {
        # If input is invalid, show error and ask again
        cat('Invalid input. Please type "y" or "n" only.\n')
      }
    }
  }

  # Connect to the SQLite database
  conn <- dbConnect(RSQLite::SQLite(), dbname = fullpath)
  
  # Write localizations data to the database, if provided
  if (!is.null(localizations_data)) {
    dbWriteTable(conn, "LOCALIZATIONS", localizations_data, overwrite=TRUE)
    message("Localizations data saved.")
  }
  
  # Write detections data to the database, if provided
  if (!is.null(detections_data)) {
    dbWriteTable(conn, "DETECTIONS", detections_data, overwrite=TRUE)
    print("Detections data saved.")
  }
  
  # Close the database connection
  dbDisconnect(conn)
}
