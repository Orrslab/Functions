#' Extract Metadata from SQLite Filename
#'
#' This function extracts the species ID, the last four digits of the tag number (converted to numeric
#' and prefixed with 97200600), the start time, and the end time from a given SQLite file name.
#'
#' @param sqlite_file_name A string representing the SQLite file name.
#' @return A list containing species_id (character), full_tag_number (numeric), 
#'         start_time (character), and end_time (character).
#'
#' @examples
#' extract_sqlite_metadata("BS_1074_from_2023-03-24_00-00-00_to_2023-03-24_23-59-57_annotated.sqlite")
#' extract_sqlite_metadata("BO_1234_from_2022-10-05_to_2022-10-06_raw.sqlite")
#'
#' @export
extract_sqlite_metadata_from_file_name <- function(sqlite_file_name) {
  # Define regex pattern to extract relevant parts
  pattern <- "^(\\w+)_([0-9]{4})_from_([0-9]{4}-[0-9]{2}-[0-9]{2}(?:_[0-9]{2}-[0-9]{2}-[0-9]{2})?)_to_([0-9]{4}-[0-9]{2}-[0-9]{2}(?:_[0-9]{2}-[0-9]{2}-[0-9]{2})?)"
  
  match <- regmatches(sqlite_file_name, regexec(pattern, sqlite_file_name))
  
  if (length(match[[1]]) < 5) {
    stop("Invalid file name format")
  }
  
  species_id <- match[[1]][2]  # Extract species ID
  tag_last_four <- as.numeric(match[[1]][3])  # Convert last four digits to numeric
  full_tag_number <- 97200600 * 10000 + tag_last_four  # Add prefix
  
  # # Format start_time and end_time by replacing underscores with spaces
  # start_time <- gsub("_", " ", match[[1]][4])
  # end_time <- gsub("_", " ", match[[1]][5])
  
  # Format start_time and end_time correctly
  start_time <- gsub("_", " ", match[[1]][4])  # Replace underscores with spaces
  start_time <- sub("(\\d{4}-\\d{2}-\\d{2}) (\\d{2})-(\\d{2})-(\\d{2})", "\\1 \\2:\\3:\\4", start_time)
  
  end_time <- gsub("_", " ", match[[1]][5])  # Replace underscores with spaces
  end_time <- sub("(\\d{4}-\\d{2}-\\d{2}) (\\d{2})-(\\d{2})-(\\d{2})", "\\1 \\2:\\3:\\4", end_time)
  
  return(list(species_id = species_id,
              tag_number = full_tag_number,
              start_time = start_time,
              end_time = end_time))
}

# # Example usage:
# sqlite_file_name <- "BS_1074_from_2023-03-24_00-00-00_to_2023-03-24_23-59-57_annotated.sqlite"
# result <- extract_sqlite_metadata_from_file_name(sqlite_file_name)
# print(result)
