# Load the required packages
library(DBI)
library(RMySQL)

#' Inspect the ATLAS database structure
#'
#' This function inspects the structure of the ATLAS database, including the names of tables,
#' columns in each table, and relationships between tables. It provides an overview of the
#' schema and can be used for documentation or schema design purposes.
#'
#' @param db_conn A \code{DBIConnection} object representing the connection to the database.
#' 
#' @return A list where each element corresponds to a table in the database. Each element is a
#' character vector of column names for that table. Additionally, the function prints out the
#' table names, column names, and relationships to the console.
#'
#' @details
#' The function retrieves all tables and columns in the connected database. It also queries
#' the database for relationships between tables using the \code{INFORMATION_SCHEMA.KEY_COLUMN_USAGE}
#' table, which is common in many SQL databases. Note that the relationships query may need to be
#' adjusted based on the specific SQL database in use.
#' 
inspect_database <- function(db_conn) {
  
  # Get list of tables
  tables <- dbListTables(db_conn)
  cat("Tables in the database:\n")
  print(tables)
  
  # Inspect each table
  schema <- list()
  for (table in tables) {
    fields <- dbListFields(db_conn, table)
    schema[[table]] <- fields
    cat("\nTable:", table, "\n")
    cat("Columns:\n")
    print(fields)
  }
  
  # # Relationships (if applicable) - This part is database specific
  # # For MySQL, you can use INFORMATION_SCHEMA to get relationships
  # # For simplicity, here’s an example of how you might query foreign keys
  # # You may need to adjust the SQL depending on the database type
  # 
  # relationships <- dbGetQuery(db_conn, "
  #   SELECT 
  #     TABLE_NAME, 
  #     COLUMN_NAME, 
  #     REFERENCED_TABLE_NAME, 
  #     REFERENCED_COLUMN_NAME 
  #   FROM 
  #     INFORMATION_SCHEMA.KEY_COLUMN_USAGE 
  #   WHERE 
  #     REFERENCED_TABLE_NAME IS NOT NULL
  # ")
  # 
  # cat("\nRelationships between tables:\n")
  # print(relationships)
  
  return(schema)
}

# The following script uses the 'inspect_database' to inspect the structure of the Harod database:

# Load the config file
source(file.path(getwd(), "config.R"))
# Load the functions that connect to the ATLAS database
source(paste0(path_to_atlas_data_analysis_repo, "ATLAS_database_connection.R"))

# Connect to the database
db_conn <- connect_to_atlas_db()

# Get the database structure
db_schema <- inspect_database(db_conn)

# Close the connection
dbDisconnect(db_conn)



