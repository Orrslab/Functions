library(DBI)

# Function to load data from a SQLite file
load_data_from_sqlite_file <- function(sqlite_file) {
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
      dbReadTable(conn, name = "LOCALIZATIONS")  # Assuming the first table contains the data
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