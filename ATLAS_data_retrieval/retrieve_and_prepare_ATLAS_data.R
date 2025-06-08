# This script runs the entire routine to get the ATLAS data and filter them

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# Get the required paths from the config file config.R
source(file.path(getwd(), "/ATLAS_data_retrieval/config.R"))

# Install the required R packages- if not yet installed
source(file.path(path_to_scripts,"install_required_R_packages.R"))

# Get the desired tag numbers and date ranges for the data retrieval
source(file.path(path_to_scripts, data_requests_file_name))

# Set the folder path to save or retrieve the atlas data sqlite files 
if (retrieve_data_from_server) {
  folder_path_to_sqlite_files <- fodler_path_to_save_sqlite_files
} else {
  folder_path_to_sqlite_files <- fodler_path_to_retrieve_sqlite_files
}

# Generate the file names from the tag numbers and dates
source(file.path(getwd(), "create_list_of_sqlite_filepaths.R"))
fullpaths_to_sqlite_files <- create_list_of_sqlite_filepaths(data_requests, 
                                                             folder_path_to_sqlite_files)

# Get the ATLAS database credentials from the config file
harod_db_credentials <- list(
  system_name = system_name_harod,         # System name
  db_username = db_username_harod,         # username
  db_pass = db_pass_harod,                 # password
  db_host_ip = db_host_ip_harod,           # host IP address
  db_port_number = db_port_number_harod,   # port number
  db_name = db_name_harod                  # database name
)

# Get the ATLAS data- either from the server, or from an SQLite file
source(file.path(getwd(),"get_ATLAS_data.R"))
raw_atlas_data = get_ATLAS_data(data_requests = data_requests, 
                                atlas_db_credentials = harod_db_credentials,
                                retrieve_data_from_server = retrieve_data_from_server,
                                save_data_to_sqlite_file = save_data_to_sqlite_file,
                                full_paths_to_sqlite_files = fullpaths_to_sqlite_files)

raw_location_data <- raw_atlas_data$LOCALIZATIONS
raw_detection_data <- raw_atlas_data$DETECTIONS

# # Calculate the confidence of each location point
# source(paste0(path_to_atlas_data_analysis_repo,"Track_cpp.R"))
# raw_data_with_confidence_levels <- TrackConfidenceLevelcpp(raw_location_data,
#                                                            conectedVel=20,
#                                                            conectedDist=NA,
#                                                            stdlim=80,
#                                                            minNBSforConf2=7,
#                                                            minNBSforConf1=4,
#                                                            Nconf1forConf2=5)
# 
# # Filter all data with conf = 2
# data__with_confidence_2 <- raw_data_with_confidence_levels %>%
#   filter(Conf == 2)

# # Save the raw data with the DAY column to sqlite
# source(paste0(path_to_atlas_data_analysis_repo, "save_ATLAS_data_to_sqlite.R"))
# save_ATLAS_data_to_sqlite(localizations_data = raw_location_data, 
#                           tag_number = 972006000836, 
#                           start_time = '2023-12-24 00:00:01',
#                           end_time = '2023-12-25 00:00:01')

# # # Assign day numbers to the data
# source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))
# # convert the time column to the POSIXct format- required for using AssignDayNumber.R
# raw_location_data$dateTime <- convert_to_POSIXct(raw_location_data$TIME)
# source(paste0(path_to_atlas_data_analysis_repo, "AssignDayNumber.R"))
# raw_location_data <- AssignDayNumber(data=raw_location_data,
#                                      DayStartTime = "23:00:00",
#                                      DayEndTime = "18:00:00",
#                                      TimeColName = "dateTime")

# raw_location_data <- AssignDayNumber(data=raw_location_data,
#                                      DayEndTime = "10:00:00",
#                                      TimeColName = "dateTime")

# # Save the raw data as CSV
# write.csv(raw_location_data, paste0(path_to_csv_files, "BO.csv"), row.names = FALSE)

# Activate the shiny Visual Filter
# source(paste0(path_to_atlas_data_analysis_repo, "visual_filter_shiny_app.R"))