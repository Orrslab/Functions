
# Load the functions that connect to the ATLAS database
source(paste0(path_to_atlas_data_analysis_repo, "ATLAS_database_connection.R"))

retrieve_and_store_atlas_data <- function(data_requests) {
  
  #connect to the database
  db_conn <- connect_to_atlas_db()
  
  for (request in data_requests) {
    
    tag_numbers <- request$tag
    start_time <- request$start_time
    end_time <- request$end_time
    
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