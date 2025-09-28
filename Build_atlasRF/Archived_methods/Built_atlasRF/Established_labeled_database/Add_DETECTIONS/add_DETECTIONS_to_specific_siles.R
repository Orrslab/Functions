
source(paste0(getwd(), "/ATLAS_data_retrieval/config.R"))
source(paste0(getwd(), "/connect_to_atlas_db.R"))
source(paste0(getwd(), "/extract_sqlite_metadata_from_file_name.R"))
source(paste0(getwd(), "/Build_atlasRF/Visual_Filter_DB_establishment/Add_DETECTIONS/add_detections_to_single_file.R"))

# Define file names
sqlite_file_names <- c(
  "BS_1074_from_2023-03-24_00-00-00_to_2023-03-24_23-59-57_annotated.sqlite",
  "GJ_0722_from_2023-02-05_00-00-00_to_2023-02-05_23-55-42_annotated.sqlite",
  "GJ_0735_from_2023-04-04_00-00-00_to_2023-04-04_23-33-41_annotated.sqlite"
)

folder_of_sqlite_files <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Missing_detections"

# Get the ATLAS database credentials from the configuration file
harod_db_credentials <- list(
  system_name = system_name_harod,         # System name
  db_username = db_username_harod,         # username
  db_pass = db_pass_harod,                 # password
  db_host_ip = db_host_ip_harod,           # host IP address
  db_port_number = db_port_number_harod,   # port number
  db_name = db_name_harod                  # database name
)

for (file_name in sqlite_file_names) {
  
  print(file_name)
  
  # Extract the species id, tag number, start and end times
  metadata <- extract_sqlite_metadata_from_file_name(file_name)
  
  # Store extracted values in separate variables
  species_id <- metadata$species_id
  tag_number <- metadata$tag_number
  start_time <- metadata$start_time
  end_time <- metadata$end_time
  
  # Connect to the ATLAS database
  atlas_db_conn <- connect_to_atlas_db(harod_db_credentials)
  
  add_detections_to_single_file(species_id = species_id,
                                tag_number = tag_number,
                                start_time = start_time,
                                end_time = end_time,
                                atlas_db_conn = atlas_db_conn,
                                path_to_sqlite_files = folder_of_sqlite_files)
  
  # Disconnect from the ATLAS database
  dbDisconnect(atlas_db_conn)
  
}