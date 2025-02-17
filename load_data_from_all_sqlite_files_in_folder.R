
load_data_from_all_sqlite_files_in_folder <- function(sqlite_folder) {
  
  library(DBI)
  library(dplyr)
  
  # Get all SQLite files in the folder
  sqlite_files <- list.files(sqlite_folder, pattern = "\\.sqlite$", full.names = TRUE)
  
  # Initialize an empty list to store dataframes
  data_list <- list()
  
  source(paste0(getwd(), "/load_data_from_sqlite_file.R"))
  
  # Iterate over SQLite files and load data
  for (file in sqlite_files) {
    cat("Loading data from:", file, "\n")
    data <- load_data_from_sqlite(file)
    
    # Append to the list if data is not NULL
    if (!is.null(data)) {
      data_list[[file]] <- data
    }
  }
  
  # Combine all dataframes into one
  if (length(data_list) > 0) {
    combined_data <- bind_rows(data_list)
  }
  
  return(combined_data)
  
}
