#' Load Specific Tables from an SQLite Database
#'
#' This function loads specified tables from an SQLite database file.
#'
#' @param sqlite_filepath A character string representing the full path to the SQLite database file.
#' @param tables A character vector of table names to be retrieved from the database.
#'
#' @return A named list of data frames, where each element corresponds to one of the requested tables.
#'         If the file does not exist, the function returns `NULL`. If a table doesn't exist in the database,
#'         it is skipped with a warning.
#'
#' @examples
#' # Example usage:
#' file_path <- "C:/data/Filtered_data/my_file.sqlite"
#' result <- load_tables_from_sqlite(file_path, c("LOCALIZATIONS", "DETECTIONS", "META"))
#'
#' if (!is.null(result$LOCALIZATIONS)) head(result$LOCALIZATIONS)

load_tables_from_sqlite_file <- function(sqlite_filepath, tables) {
  
  # Load required packages
  required_packages <- c("RSQLite", "DBI")
  invisible(lapply(required_packages, library, character.only = TRUE))
  
  # Check if the file exists
  if (!file.exists(sqlite_filepath)) {
    warning(paste0("The file '", basename(sqlite_filepath), "' does not exist."))
    return(NULL)
  }
  
  # Connect to the database
  conn <- dbConnect(RSQLite::SQLite(), dbname = sqlite_filepath)
  
  # Get list of available tables in the database
  available_tables <- dbListTables(conn)
  
  # Initialize result list
  result_list <- list()
  
  # Loop over requested tables
  for (table_name in tables) {
    if (table_name %in% available_tables) {
      result_list[[table_name]] <- dbReadTable(conn, table_name)
    } else {
      warning(paste0("Table '", table_name, "' does not exist in the database '", basename(sqlite_filepath), "'."))
    }
  }
  
  # Disconnect
  dbDisconnect(conn)
  
  return(result_list)
}