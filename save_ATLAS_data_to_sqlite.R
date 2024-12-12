# Load required packages
required_packages <- c("RSQLite", "DBI", "dplyr", "crayon")
sapply(required_packages, library, character.only = TRUE)

#' Save ATLAS Data to SQLite Database
#'
#' This function saves localization and detection data to an SQLite database. 
#' The user can specify the file path and whether to overwrite existing files. 
#' Data is saved into separate tables named `LOCALIZATIONS` and `DETECTIONS`.
#'
#' @param localizations_data A data frame containing localization data. 
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
#'     the corresponding data frames (`localizations_data` or `detections_data`) 
#'     are provided.
#'   - The database connection is automatically closed after the operation is complete.
#'
#' @examples
#' # Example data
#' localizations_data <- data.frame(
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
#'   localizations_data = localizations_data,
#'   detections_data = detections_data,
#'   fullpath = "atlas_data.sqlite"
#' )
#'
#' @seealso \code{\link{dbConnect}} and \code{\link{dbWriteTable}} from the 
#'   `RSQLite` package for database operations.
#'          
save_ATLAS_data_to_sqlite <- function(localizations_data=NULL, detections_data=NULL, fullpath = getwd())
{
  
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
  if (!is.null(localizations_data)) {
    dbWriteTable(conn, "LOCALIZATIONS", localizations_data, overwrite=TRUE)
    message("Localizations data saved.")
  }
  
  # Write detections data to the database, if provided
  if (!is.null(detections_data)) {
    dbWriteTable(conn, "DETECTIONS", detections_data, overwrite=TRUE)
    print("Detections data saved.")
  }
  
  # Create and insert data into the PROPERTIES table
  properties_data <- data.frame(KEY = "atlas-system", VALUE = "harod")
  dbWriteTable(conn, "PROPERTIES", properties_data, overwrite=TRUE)
  message("PROPERTIES table added.")
  
  # Close the database connection
  dbDisconnect(conn)
}
