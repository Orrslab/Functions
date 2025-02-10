library(DBI)
library(RSQLite)

get_sqlite_table_names <- function(sqlite_file) {
  # Check if file exists
  if (!file.exists(sqlite_file)) {
    stop("The specified SQLite file does not exist.")
  }
  
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), sqlite_file)
  
  # Get table names
  table_names <- dbListTables(con)
  
  # Disconnect from database
  dbDisconnect(con)
  
  return(table_names)
}

# # Example of usage
# sqlite_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Raw_data/GJ_0712_from_2022-03-19_02-00-00_to_2022-04-07_03-00-00_raw.sqlite"
# message("Table names in the inspected sqlite file:")
# print(get_sqlite_table_names(sqlite_path))