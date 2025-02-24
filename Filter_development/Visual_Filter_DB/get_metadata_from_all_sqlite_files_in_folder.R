
get_metadata_from_all_sqlite_files_in_folder <- function(sqlite_folder) {
  
  library(DBI)
  library(dplyr)
  
  # Get all SQLite files in the folder
  sqlite_files <- list.files(sqlite_folder, pattern = "\\.sqlite$", full.names = TRUE)
  
  # Initialize an empty list to store metadata
  metadata_list <- list()
  
  source(paste0(getwd(), "/load_data_from_sqlite_file.R"))
  
  # Iterate over SQLite files and load data
  for (file in sqlite_files) {
    cat("Loading data from:", file, "\n")
    data <- load_data_from_sqlite_file(file)
    
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
  }
  
  return(files_metadata)
}