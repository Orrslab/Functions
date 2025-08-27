# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(readxl)
library("dplyr")
library(lubridate)

source(file.path(getwd(), "ATLAS_data_retrieval/config_beacons.R"))
source(file.path(getwd(), "connect_to_atlas_db.R"))

# ### USER'S INPUT BEGIN
path_to_beacons_info <- "PATH/TO/BEACONS/METADATA/FILE"

atlas_system_name <- "your atlas system name"
atlas_db_username <- "your atlas db username"
atlas_db_pass <- "your atlas db password"
atlas_db_host_ip <- "your atlas db host ip"
atlas_db_port_number <- "your atlas db port number"
atlas_db_name <- "your atlas db name"

folder_to_save_the_beacons_data <- file.path(getwd(), "Filter_development/Feature_engineering/Beacon_detections_data")
# ### USER'S INPUT END

# Load the beacon tag numbers and dates from the info file
beacons_info <- read_excel(path_to_beacons_info)

# Ensure consistent locale for parsing- without it the code does not convert September dates correctly...
Sys.setlocale("LC_TIME", "C")

# Get the ATLAS database credentials from the config file
atlas_db_credentials <- list(
  system_name = atlas_system_name,         # System name
  db_username = atlas_db_username,         # username
  db_pass = atlas_db_pass,                 # password
  db_host_ip = atlas_db_host_ip,           # host IP address
  db_port_number = atlas_db_port_number,   # port number
  db_name = atlas_db_name                  # database name
)

# Connect to the ATLAS database (server)
db_conn <- connect_to_atlas_db(atlas_db_credentials = atlas__db_credentials)

# Initiate an empty list which will store all the base station summary data
all_data_bs_summaries <- list()

for (i in seq_len(nrow(beacons_info))) {
  
  tag_number <- beacons_info$TAG[i]
  start_time <- beacons_info$Since[i]
  end_time <- beacons_info$Until[i]
  
  ## Convert the start and end time to round hour:
  
  # Convert the time to POSIXct
  start_time_posix <- as.POSIXct(start_time, format = beacons_time_format, tz = beacons_time_zone)
  if (end_time == "Infinity") {
    end_time_posix <- as.POSIXct(infinity_time, format = beacons_time_format, tz = beacons_time_zone)
  } else {
    end_time_posix <- as.POSIXct(end_time, format = beacons_time_format, tz = beacons_time_zone)
  }
    
  # Round down to hour
  start_time_rounded <- floor_date(start_time_posix, unit = "hour")
  end_time_rounded <- floor_date(end_time_posix, unit = "hour")
  
  # Convert to a unix time stamp in miliseconds
  start_time_rounded <- as.numeric(start_time_rounded) * 1000
  end_time_rounded   <- as.numeric(end_time_rounded)   * 1000
  
  # Build the query
  # query <- paste('SELECT * from HOURLY_TAG_BS_SUMMARIES WHERE TAG =', tag_number)
  
  # query <- paste('SELECT * from HOURLY_TAG_BS_SUMMARIES WHERE TAG =', tag_number,
  #                "AND HOUR >=", start_time_rounded,
  #                "AND HOUR <=", end_time_rounded)
  
  query <- paste0(
    "SELECT * FROM HOURLY_TAG_BS_SUMMARIES WHERE TAG = ", 
    format(tag_number, scientific = FALSE),
    " AND HOUR >= ", format(start_time_rounded, scientific = FALSE),
    " AND HOUR <= ", format(end_time_rounded, scientific = FALSE)
  )
  
  print(query)
  
  bs_summary_data <- dbGetQuery(db_conn,query)
  
  # Add the base stations summary segment to the list with the entire data
  if (!is.null(bs_summary_data) && nrow(bs_summary_data) > 0) {
    all_data_bs_summaries[[length(all_data_bs_summaries) + 1]] <- bs_summary_data
  }
  
}

# Disconnect from the database
dbDisconnect(db_conn)

# Bind togther the data from all tags
base_stations_summary_per_tag <- if (length(all_data_bs_summaries) > 0) {
  bind_rows(all_data_bs_summaries)
} else {
  NULL  # No detection data retrieved
}

saveRDS(base_stations_summary_per_tag, file = file.path(folder_to_save_the_beacons_data, "base_stations_summary_per_beacon.Rds"))
