
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

source(file.path(getwd(), "Data_from_ATLAS_server.R"))
source(file.path(getwd(), "connect_to_atlas_db.R"))
source(file.path(getwd(), "ATLAS_data_retrieval/config.R"))

# Get the ATLAS database credentials from the config file
harod_db_credentials <- list(
  system_name = system_name_harod,         # System name
  db_username = db_username_harod,         # username
  db_pass = db_pass_harod,                 # password
  db_host_ip = db_host_ip_harod,           # host IP address
  db_port_number = db_port_number_harod,   # port number
  db_name = db_name_harod                  # database name
)

atlas_conn <- connect_to_atlas_db(atlas_db_credentials = harod_db_credentials)

atlas_data <- data_from_atlas_server(Start_Time_Str = "2024-08-15 06:01:00",
                       End_Time_Str = "2024-08-15 06:03:00",
                       Tag_numbers = 972006001508,
                       includeDet=TRUE,
                       includeLoc=TRUE, 
                       dbc = atlas_conn)

detections_data <- atlas_data$DETECTIONS

localization_data <- atlas_data$LOCALIZATIONS
