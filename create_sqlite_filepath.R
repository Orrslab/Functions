
#' Create the SQLite File Path from Tag Numbers and Date Range
#'
#' This function generates the file path for the SQLite database based on the 
#' provided tag numbers and date range. If a single tag number is provided, 
#' the last four digits of the tag number will be included in the filename. 
#' If multiple tag numbers are provided, the filename will include "Multiple_tags". 
#' The start and end times are formatted by replacing spaces with underscores 
#' and colons with hyphens.
#'
#' @param animal_name_code A string representing the animal name code used in 
#'                         the file name (e.g., "BO" for Barn Owl).
#' @param tag_numbers A vector of tag numbers (either a single number or multiple). 
#'                    The last four digits of the tag number will be included 
#'                    in the filename if a single tag is provided, or "Multiple_tags" 
#'                    will be used if there are multiple tags.
#' @param start_time A string representing the start time in 'YYYY-MM-DD HH:MM:SS' format. 
#'                   The function will replace spaces with underscores and colons with hyphens.
#' @param end_time A string representing the end time in 'YYYY-MM-DD HH:MM:SS' format. 
#'                 The function will replace spaces with underscores and colons with hyphens.
#' @param folder_path_to_sqlite_files A string representing the folder path where the SQLite 
#'                                    files are located.
#'
#' @return A string representing the full file path for the SQLite database, 
#'         including the animal name code, tag number(s), and formatted date range.
#'
#' @examples
#' \dontrun{
#' # Example for a single tag
#' create_sqlite_filepath("BO", 972006000556, "2021-07-01 08:00:00", "2021-07-01 10:00:00", 
#'                        "C:/data/filtered_data")
#'
#' # Example for multiple tags
#' create_sqlite_filepath("BO", c(972006000556, 972006000430), "2021-07-01 08:00:00", "2021-07-01 10:00:00", 
#'                        "C:/data/filtered_data")
#' }
#'
#' @export
#' 
create_sqlite_filepath <- function(animal_name_code, tag_numbers, start_time, end_time, folder_path_to_sqlite_files) {
  if (length(tag_numbers) == 1) {
    # Take the last four digits of the tag number
    tag_number_str <- substr(as.character(tag_numbers), nchar(tag_numbers)-3, nchar(tag_numbers))
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
  filename <- paste(animal_name_code, "_",
                    tag_number_str, "_from_", 
                    start_time_replace_colons, "_to_", 
                    end_time_replace_colons, ".sqlite", sep = "")
  filepath = paste0(folder_path_to_sqlite_files, "/", filename)

  return(filepath)
}