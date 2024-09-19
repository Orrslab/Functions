
source(file.path(getwd(), "Scripts", "config.R"))


# Download ATLAS data directly from the server
# This requires a VPN connection to the TAU server
source(paste0(path_to_atlas_data_analysis_repo, "Data_from_ATLAS_server.R"))

AllData <- Data_from_ATLAS_server(Start_Time_Str,
                                  End_Time_Str,
                                  tag_numbers, 
                                  includeDet=FALSE)

RawLoc0 <- AllData$LOC
RawDet0 <- AllData$DET

source(paste0(path_to_atlas_data_analysis_repo, "save_ATLAS_data_to_sqlite.R"))

print(1)

save_ATLAS_data_to_sqlite(localizations_data = RawLoc0, 
                          tag_number = tag_numbers, 
                          start_time = Start_Time_Str,
                          end_time = End_Time_Str)

  


