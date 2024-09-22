
# Load the functions that connect to the ATLAS database
source(paste0(path_to_atlas_data_analysis_repo, "ATLAS_database_connection.R"))

retrieve_and_store_atlas_data <- function(data_requests, SYS=system_name_harod) {
  
  # Get the credentials of the ATLAS database you need to connect to
  
  if (SYS==system_name_harod) {
    # Get the Harod database credentials from the configuration file
    db_username = db_username_harod         # username
    db_pass = db_pass_harod                 # password
    db_host_ip = db_host_ip_harod           # host ip address
    db_port_number = db_port_number_harod   # port Number
    db_name = db_name_harod                 # name of data base
    
  } else {
    stop(glue("Warning: system {SYS} is not defined"))
  }
  
  #connect to the database
  db_conn <- connect_to_atlas_db(db_username, 
                                 db_pass, 
                                 db_host_ip, 
                                 db_port_number, 
                                 db_name)
  
  for (request in data_requests) {
    
    tag_numbers <- request$tag
    start_time <- request$start_time
    end_time <- request$end_time
    
    print(start_time)
    print(end_time)
    
    source(paste0(path_to_atlas_data_analysis_repo, "Data_from_ATLAS_server.R"))
    AllData <- Data_from_ATLAS_server(start_time,
                                      end_time,
                                      tag_numbers,
                                      includeLoc = TRUE,
                                      includeDet=FALSE,
                                      db_conn)
    
    RawLoc0 <- AllData$LOC
    RawDet0 <- AllData$DET
    
    if (save_data_to_sqlite_file) {
      
      source(paste0(path_to_atlas_data_analysis_repo, "save_ATLAS_data_to_sqlite.R"))
      
      save_ATLAS_data_to_sqlite(localizations_data = RawLoc0, 
                                tag_number = tag_numbers, 
                                start_time = start_time,
                                end_time = end_time)
    }
  }
  
  # Disconnect from the database
  disconnect_from_db(db_conn)
  
  # Return the last retrieved localizations table
  return(RawLoc0)
}