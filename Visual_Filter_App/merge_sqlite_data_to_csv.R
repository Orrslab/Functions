# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# Load required libraries
library(DBI)
library(dplyr)

### USER'S INPUT REQUIRED ###
# Define folder containing SQLite files:
sqlite_folder <- "DEFINE/HERE/THE/PATH/TO/SQLITE/Annotated_data"  
### END OF USER'S INPUT ###

# Function to load data from a SQLite file
load_data_from_sqlite <- function(sqlite_file) {
  # Try to connect to the SQLite database
  conn <- tryCatch({
    dbConnect(RSQLite::SQLite(), sqlite_file)
  }, error = function(e) {
    cat("Error connecting to:", sqlite_file, "\n", e$message, "\n")
    return(NULL)
  })
  
  # If the connection failed, return NULL
  if (is.null(conn)) {
    return(NULL)
  }
  
  # Try to get table names and read data
  data <- tryCatch({
    tables <- dbListTables(conn)
    if (length(tables) > 0) {
      dbReadTable(conn, tables[1])  # Assuming the first table contains the data
    } else {
      cat("No tables found in:", sqlite_file, "\n")
      NULL
    }
  }, error = function(e) {
    cat("Error reading data from:", sqlite_file, "\n", e$message, "\n")
    NULL
  })
  
  # Disconnect from the database
  dbDisconnect(conn)
  
  return(data)
}

# Get all SQLite files in the folder
sqlite_files <- list.files(sqlite_folder, pattern = "\\.sqlite$", full.names = TRUE)

# Initialize an empty list to store dataframes
data_list <- list()

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
  
  # Evaluate date range
  if ("dateTime" %in% colnames(combined_data)) {
    combined_data$dateTime <- as.POSIXct(combined_data$dateTime, origin = "1970-01-01", tz = "UTC")
    
    start_time <- min(combined_data$dateTime, na.rm = TRUE)
    end_time <- max(combined_data$dateTime, na.rm = TRUE)
    
    # Format dates for the filename
    start_time_str <- format(start_time, "%Y-%m-%d_%H-%M-%S")
    end_time_str <- format(end_time, "%Y-%m-%d_%H-%M-%S")
    
    # Dynamically create the output file name
    output_csv_file_name <- paste0(
      "combined_annotated_data_from_", 
      start_time_str, 
      "_to_", 
      end_time_str, 
      ".csv"
    )
    
    # Save the combined dataframe as a CSV file
    write.csv(combined_data, paste0(sqlite_folder, '/', output_csv_file_name), row.names = FALSE)
    message("Combined data saved to:", output_csv_file_name, "\n")
  } else {
    message("No 'timestamp' column found in the data. Cannot determine date range.\n")
  }
} else {
  cat("No data was loaded from the SQLite files.\n")
}