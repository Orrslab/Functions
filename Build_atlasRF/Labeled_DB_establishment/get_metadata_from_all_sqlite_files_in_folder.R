library(DBI)
library(dplyr)

#' Get Metadata_per_file from All SQLite Files in a Folder
#'
#' This function retrieves metadata (such as tag number, start time, end time, and number of records) 
#' from all SQLite files within a specified folder. The function loads data from each SQLite file,
#' processes the `TIME` column, and extracts relevant metadata for each file.
#'
#' @param sqlite_folder A character string specifying the path to the folder containing SQLite files of annotated data of a certain species.
#'
#' @return A data frame with the metadata for each SQLite file in the folder. The data frame contains
#'   columns:
#'   \item{TAG}{Unique tag number extracted from the data.}
#'   \item{Start_time}{Earliest timestamp (start time) in the dataset, converted to a human-readable datetime.}
#'   \item{End_time}{Latest timestamp (end time) in the dataset, converted to a human-readable datetime.}
#'   \item{Num_records}{The total number of rows (data points) in the dataset.
#'   \item{Data_source}{For example, ATLAS system Harod.
#'   \item{Reviewer}{The name of the person who annotated outliers in the data.
#'   \item{Filter_applied}{The tool used to annotate outliers in the data, for example Visual Filter App.}
#'
#' @import DBI
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'   metadata <- get_metadata_from_all_sqlite_files_in_folder("path/to/folder")
#'   head(metadata)
#' }
#'
#' @export

get_metadata_from_all_sqlite_files_in_folder <- function(sqlite_folder) {
  
  # Get all SQLite files in the folder
  sqlite_files <- list.files(sqlite_folder, pattern = "\\.sqlite$", full.names = TRUE)
  
  # Initialize an empty list to store metadata
  metadata_list <- list()
  
  source(paste0(getwd(), "/load_localization_data_from_sqlite_file.R"))
  
  # Iterate over SQLite files and load data
  for (file in sqlite_files) {
    # cat("Loading data from:", file, "\n")
    data <- load_localization_data_from_sqlite_file(file)
    
    # Convert `TIME` from milliseconds to human-readable datetime
    data$dateTime <- as.POSIXct(data$TIME / 1000, origin = "1970-01-01", tz = "UTC")
    
    metadata <- data.frame(
      TAG = unique(data$TAG),       # Extracts the unique tag number
      Start_time = min(data$dateTime),  # Earliest time entry
      End_time = max(data$dateTime),    # Latest time entry
      Num_records = nrow(data)      # Total number of rows (points)
    )
    
    metadata_list[[file]] <- metadata
    
  }
  
  # Combine all dataframes into one
  if (length(metadata_list) > 0) {
    files_metadata <- bind_rows(metadata_list)
  } else {
    files_metadata <- data.frame()  # return empty df if no files found
  }
  
  return(files_metadata)
}