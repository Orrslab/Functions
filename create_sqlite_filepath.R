
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
#' @param path_to_sqlite_files The path that leads to the sqlite files
#'
#' @return A character string representing the file path for the SQLite 
#'         database, including the formatted tag numbers and date range.
#'  
create_sqlite_filepath <- function(tag_numbers, start_time, end_time, path_to_sqlite_files) {
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