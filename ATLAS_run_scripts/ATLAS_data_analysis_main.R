# This script runs the entire routine to get the ATLAS data and filter them

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# Get the required paths from the config file config.R
source(file.path(getwd(), "config.R"))

# Install the required R packages- in not yet installed
source(paste0(path_to_scripts,"install_required_R_packages.R"))

# Get the desired tag numbers and date ranges for the data retrieval
source(paste0(path_to_scripts, data_requests_file_name))

# Get the ATLAS data- either from the server, or from an SQLite file
source(paste0(path_to_atlas_data_analysis_repo,"get_ATLAS_data.R"))
raw_location_data = get_ATLAS_data()

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

# Assign day numbers to the data
source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))
# convert the time column to the POSIXct format- required for using AssignDayNumber.R
raw_location_data$dateTime <- convert_to_POSIXct(raw_location_data$TIME)
source(paste0(path_to_atlas_data_analysis_repo, "AssignDayNumber.R"))
raw_location_data <- AssignDayNumber(data=raw_location_data, TimeColName = "dateTime")

# Save the raw data as CSV
write.csv(raw_location_data, paste0(path_to_csv_files, "BO_0836.csv"), row.names = FALSE)

# Activate the shiny Visual Filter
# source(paste0(path_to_atlas_data_analysis_repo, "visual_filter_shiny_app.R"))