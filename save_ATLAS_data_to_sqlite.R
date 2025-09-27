#' Save ATLAS Data to SQLite Database
#'
#' This function saves localization and detection data to an SQLite database. 
#' The user can specify the file path and whether to overwrite existing files. 
#' Data is saved into separate tables named `LOCALIZATIONS` and `DETECTIONS`.
#'
#' @param localization_data A data frame containing localization data. 
#'   Default is `NULL`. If `NULL`, the localization data table will not be created.
#' @param detections_data A data frame containing detection data. 
#'   Default is `NULL`. If `NULL`, the detection data table will not be created.
#' @param fullpath A character string specifying the full path (including file name) 
#'   where the SQLite database should be saved. Default is the current working directory (`getwd()`).
#'
#' @return This function does not return any value. It writes the provided data 
#'   frames to the specified SQLite database file.
#'
#' @details 
#'   - If a file with the same name already exists, the function prompts the user 
#'     to confirm whether to overwrite it. If the user chooses not to overwrite, 
#'     the function exits without saving the data.
#'   - The database tables `LOCALIZATIONS` and `DETECTIONS` are created only if 
#'     the corresponding data frames (`localization_data` or `detections_data`) 
#'     are provided.
#'   - The database connection is automatically closed after the operation is complete.
#'
#' @examples
#' # Example data
#' localization_data <- data.frame(
#'   tag = c("972006000430", "972006000431"),
#'   time = c("2024-01-01 12:00:00", "2024-01-01 12:05:00"),
#'   x = c(1.234, 2.345),
#'   y = c(3.456, 4.567)
#' )
#'
#' detections_data <- data.frame(
#'   tag = c("972006000430", "972006000431"),
#'   signal_strength = c(95, 90),
#'   detection_time = c("2024-01-01 12:00:00", "2024-01-01 12:05:00")
#' )
#'
#' # Save data to SQLite
#' save_ATLAS_data_to_sqlite(
#'   localization_data = localization_data,
#'   detections_data = detections_data,
#'   fullpath = "atlas_data.sqlite"
#' )
#'
#' @seealso \code{\link{dbConnect}} and \code{\link{dbWriteTable}} from the 
#'   `RSQLite` package for database operations.
#'          
save_ATLAS_data_to_sqlite <- function(localization_data=NULL, detections_data=NULL, fullpath = getwd())
{
  # Load required packages
  required_packages <- c("RSQLite", "DBI", "dplyr", "crayon")
  invisible(lapply(required_packages, library, character.only = TRUE))
  
  fullpath <- as.character(fullpath)
  
  # # If a file with the same name exists, ask the user whether to overwrite it
  # if (file.exists(fullpath)) {
  #   repeat {
  #     cat(green("The file: \n'", basename(fullpath), 
  #               "' already exists.\n", "Do you want to overwrite it? (y/n): \n"))
  #     user_input <- readline()
  #     
  #     if (tolower(user_input) == "y") {
  #       # If 'y', proceed with overwriting
  #       break
  #     } else if (tolower(user_input) == "n") {
  #       # If 'n', stop the function or print a message
  #       message("Data will not be saved into sqlite.")
  #       return()
  #     } else {
  #       # If input is invalid, show error and ask again
  #       cat('Invalid input. Please type "y" or "n" only.\n')
  #     }
  #   }
  # }

  # Connect to the SQLite database
  conn <- dbConnect(RSQLite::SQLite(), dbname = fullpath)
  
  # Write localizations data to the database, if provided
  if (!is.null(localization_data)) {
    
    dbWriteTable(conn, "LOCALIZATIONS", localization_data, overwrite=TRUE)
    message("Localizations data saved as sqlite.")
    
  }
  
  # Write detections data to the database, if provided
  if (!is.null(detections_data)) {
    
    dbWriteTable(conn, "DETECTIONS", detections_data, overwrite=TRUE)
    message("Detections data saved as sqlite.")
    
  # } else {
  #   
  #   # Create an empty DETECTIONS table with the specified schema- 
  #   # This is necessary in order to be able to upload the saved sqlite file into Kamadata
  #
  #   create_table_query <- "
  #   CREATE TABLE IF NOT EXISTS DETECTIONS (
  #     BS INTEGER,
  #     TAG BIGINT,
  #     TX BIGINT,
  #     TIME BIGINT,
  #     SAMPLES_CLK REAL,
  #     SNR REAL,
  #     RSSI REAL,
  #     HEADROOM REAL,
  #     GAIN REAL
  #   );
  #   "
  #   
  #   # Execute the query to create the table
  #   dbExecute(conn, create_table_query)
    
  }
  
  # Create and insert data into the PROPERTIES table
  properties_data <- data.frame(KEY = "atlas-system", VALUE = "harod")
  dbWriteTable(conn, "PROPERTIES", properties_data, overwrite=TRUE)
  
  # Close the database connection
  dbDisconnect(conn)
}
