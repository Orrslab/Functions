
source(file.path(getwd(), "Scripts", "config.R"))


if (load_data_from_sqlite_file) {
  
  source(paste0(path_to_atlas_data_analysis_repo, "create_sqlite_filepath.R"))
  filepath <- create_sqlite_filepath(tag_numbers, Start_Time_Str, End_Time_Str)
  
  source(paste0(path_to_atlas_data_analysis_repo, "load_atlas_data_from_sqlite.R"))
  RawLoc0 <- load_atlas_data_from_sqlite(filepath)
  
  
} else {
  
  # Download ATLAS data directly from the server
  # This requires a VPN connection to the TAU server

  source(paste0(path_to_atlas_data_analysis_repo, "retrieve_and_store_atlas_data.R"))
  RawLoc0 <- retrieve_and_store_atlas_data(data_requests)
}


  


