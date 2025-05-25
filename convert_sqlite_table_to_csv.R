convert_sqlite_table_to_csv <- function(sqlite_path, table_name = "LOCALIZATIONS", output_csv_path) {
  # Load required packages
  if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
  if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
  
  library(DBI)
  library(RSQLite)
  
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), sqlite_path)
  
  # Check if the table exists
  if (!table_name %in% dbListTables(con)) {
    dbDisconnect(con)
    stop(paste("Table", table_name, "does not exist in the database."))
  }
  
  # Read the table
  data <- dbReadTable(con, table_name)
  
  # Write to CSV
  write.csv(data, file = output_csv_path, row.names = FALSE)
  
  # Disconnect from the database
  dbDisconnect(con)
  
  cat("Saved", table_name, "to", output_csv_path, "\n")
}