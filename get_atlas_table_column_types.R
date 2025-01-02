# Function to get column types of a specific table
get_atlas_table_column_types <- function(db_conn, table_name) {
  query <- paste0(
    "SELECT COLUMN_NAME, DATA_TYPE 
     FROM INFORMATION_SCHEMA.COLUMNS 
     WHERE TABLE_SCHEMA = DATABASE() 
       AND TABLE_NAME = '", table_name, "'"
  )
  result <- dbGetQuery(db_conn, query)
  return(result)
}

## Example usage for the DETECTIONS table

# Load the config file with the atlas database credentials
source(file.path(getwd(), "config.R"))

# Connect to the database
source(paste0(path_to_atlas_data_analysis_repo, "connect_to_atlas_db.R"))
db_conn <- connect_to_atlas_db(db_username = db_username_harod,         # username
                               db_pass = db_pass_harod,                 # password
                               db_host_ip = db_host_ip_harod,           # host ip address
                               db_port_number = db_port_number_harod,   # port Number
                               db_name = db_name_harod)

column_types <- get_atlas_table_column_types(db_conn, "DETECTIONS")
print(column_types)