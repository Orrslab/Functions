# Retrieve DETECTIONS data from the ATLAS server and add to the specified sqlite files

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

library(readxl)

source(paste0(getwd(), "/ATLAS_data_retrieval/config.R"))
source(paste0(getwd(), "/connect_to_atlas_db.R"))
source(paste0(getwd(), "/Data_from_ATLAS_server.R"))
source(paste0(getwd(), "/create_sqlite_filepath.R"))
source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))

# Specify the necessary paths
path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB"
path_to_species_metadata <- paste0(path_to_db, "/Species_metadata.xlsx")

# Read the species data from the species metadata file
species_data <- read_excel(path_to_species_metadata)

# Get the species names list
species_id_codes <- species_data$Species_ID

# Get the ATLAS database credentials from the configuration file
harod_db_credentials <- list(
  system_name = system_name_harod,         # System name
  db_username = db_username_harod,         # username
  db_pass = db_pass_harod,                 # password
  db_host_ip = db_host_ip_harod,           # host IP address
  db_port_number = db_port_number_harod,   # port number
  db_name = db_name_harod                  # database name
)

message("Retrieving DETECTIONS data from the ATLAS server")

for (species_id in species_id_codes) {
  
  # Open the files metadata of the species sqlie files
  path_to_species_folder <- paste0(path_to_db, "/", species_id)
  files_metadata <- read.csv(paste0(path_to_species_folder, "/", species_id, "_files_metadata.csv"))
  
  # Get the tag number, start and end times of all files 
  tag_numbers <- files_metadata$TAG
  start_times <- files_metadata$Start_time
  end_times <- files_metadata$End_time
  
  # Connect to the ATLAS database
  db_conn <- connect_to_atlas_db(harod_db_credentials)
  
  # Ensure disconnection when done
  on.exit(dbDisconnect(db_conn))
  
  print(species_id)
  
  for (i in seq_along(tag_numbers)) {
    
    tag_number <- tag_numbers[i]
    start_time <- start_times[i]
    end_time <- end_times[i]
    
    # Retrieve the DETECTIONS from the ATLAS server (VPN connection is required)
    atlas_data <- data_from_atlas_server(Start_Time_Str = start_time,
                                         End_Time_Str = end_time,
                                         Tag_numbers = tag_number,
                                         includeDet = TRUE,
                                         includeLoc = FALSE, 
                                         dbc = db_conn)
    
    print(i)
    
    detections_data <- atlas_data$DETECTIONS
    
    # Get the file name and full path of the sqlite file to which the DETECTIONS data should be added
    path_to_sqlite_file <- create_sqlite_filepath(animal_name_code = species_id, 
                                                  tag_numbers = tag_number, 
                                                  start_time = as.POSIXct(sub("\\.\\d+$", "", start_time), format = "%Y-%m-%d %H:%M:%S"), 
                                                  end_time = as.POSIXct(sub("\\.\\d+$", "", end_time), format = "%Y-%m-%d %H:%M:%S"), 
                                                  folder_path_to_sqlite_files = path_to_species_folder)
    
    # Append "_annotated" before ".sqlite"
    path_to_sqlite_file <- sub("\\.sqlite$", "_annotated.sqlite", path_to_sqlite_file)
    
    # Save the DETECTIONS data into the sqlite file
    save_ATLAS_data_to_sqlite(detections_data = detections_data,
                              fullpath = path_to_sqlite_file)
    
  }
  
}
