
source(file.path(getwd(), "Scripts", "config.R"))


if (load_data_from_sqlite_file) {
  
  source(paste0(path_to_atlas_data_analysis_repo, "create_sqlite_filepath.R"))
  filepath <- create_sqlite_filepath(tag_numbers, Start_Time_Str, End_Time_Str)
  
  source(paste0(path_to_atlas_data_analysis_repo, "load_atlas_data_from_sqlite.R"))
  RawLoc0 <- load_atlas_data_from_sqlite(filepath)
  
  
} else {
  
  # Download ATLAS data directly from the server
  # This requires a VPN connection to the TAU server
  source(paste0(path_to_atlas_data_analysis_repo, "Data_from_ATLAS_server.R"))
  
  AllData <- Data_from_ATLAS_server(Start_Time_Str,
                                    End_Time_Str,
                                    tag_numbers, 
                                    includeDet=FALSE)
  
  RawLoc0 <- AllData$LOC
  RawDet0 <- AllData$DET
  
  if (save_data_to_sqlite_file) {
    
    source(paste0(path_to_atlas_data_analysis_repo, "save_ATLAS_data_to_sqlite.R"))
    
    save_ATLAS_data_to_sqlite(localizations_data = RawLoc0, 
                              tag_number = tag_numbers, 
                              start_time = Start_Time_Str,
                              end_time = End_Time_Str)
  }
}


  


