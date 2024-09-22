# Load required packages
required_packages <- c("RSQLite", "DBI", "dplyr")
sapply(required_packages, library, character.only = TRUE)

source(file.path(getwd(), "Scripts", "config.R"))

#' Create the SQLite file path from tag numbers and date range
#'
#' This function generates a file path for the SQLite database based on the 
#' provided tag numbers and the date range of the data. If there is a single 
#' tag, the last four digits of the tag number will be used in the filename. 
#' If there are multiple tags, "Multiple_tags" will be used instead. 
#' The function also formats the date and time by replacing spaces with 
#' underscores and colons with hyphens.
#'
#' @param tag_numbers A unique identifier (or identifiers) for the tag(s) 
#'                    associated with the data. If a single tag number is 
#'                    provided, the filename will include the last four digits 
#'                    of the tag number. If multiple tags are provided, the 
#'                    filename will include "Multiple_tags".
#' @param start_time A character string representing the start time in 
#'                   the format 'YYYY-MM-DD HH:MM:SS'. The function will 
#'                   format this by replacing spaces with underscores and 
#'                   colons with hyphens.
#' @param end_time A character string representing the end time in 
#'                 the format 'YYYY-MM-DD HH:MM:SS'. The function will 
#'                 format this by replacing spaces with underscores and 
#'                 colons with hyphens.
#'
#' @return A character string representing the file path for the SQLite 
#'         database, including the formatted tag numbers and date range.
#'  
create_sqlite_filepath <- function(tag_numbers, start_time, end_time) {
  
  if (length(tag_numbers) == 1) {
    # Take the last four digits of the tag number
    tag_number_str <- substr(as.character(tag_numbers), nchar(tag_numbers)-3, nchar(tag_numbers))
    tag_number_str <- paste0("Tag_", tag_number_str)
  } else {
    tag_number_str = "Multiple_tags"
  }
  
  # Replace spaces with underscores
  start_time_replace_spaces <- gsub(" ", "_", start_time)
  end_time_replace_spaces <- gsub(" ", "_", end_time)
  
  # Replace colons with minus signs
  start_time_replace_colons <- gsub(":", "-", start_time_replace_spaces)
  end_time_replace_colons <- gsub(":", "-", end_time_replace_spaces)
  
  # Create the .sqlite file name
  filename <- paste(tag_number_str, "_from_", 
                    start_time_replace_colons, "_to_", 
                    end_time_replace_colons, ".sqlite", sep = "")
  filepath = paste0(path_to_sqlite_files, filename)
  return(filepath)
}


#' Save ATLAS Data to SQLite Database
#'
#' This function saves localization and detection data to an SQLite database. 
#' The database file is named based on the provided tag number(s), start time, 
#' and end time. The function calls `create_sqlite_filepath` to generate the 
#' appropriate filename and file path.
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
#' @return This function does not return any value. It saves the provided 
#'         data frames to the specified SQLite database file.
#'
#' @details The function connects to an SQLite database and writes the 
#'          localization and detection data to separate tables. If either 
#'          data frame is not provided, the corresponding table will not be created.
#'          The database connection is closed after the operation is complete.
#'          
save_ATLAS_data_to_sqlite <- function(localizations_data=NULL, detections_data=NULL, tag_numbers, start_time, end_time)
{
  filepath <- create_sqlite_filepath(tag_numbers, start_time, end_time)

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
