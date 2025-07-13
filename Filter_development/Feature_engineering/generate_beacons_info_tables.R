# Generate the master tables of information from the beacons data

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(readxl)

source(file.path(getwd(), "Filter_development/Feature_engineering/generate_beacons_base_station_summary_table_over_time_periods.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/generate_bs_hour_detection_rate_table_from_beacons.R"))

### USER'S INPUT BEGIN
folder_to_save_the_beacons_data <- file.path(getwd(), "Filter_development/Feature_engineering")
path_to_atlas_info <- "C:/Users/netat/Documents/Movement_Ecology/ATLAS/Base_stations_beacons_info"
path_to_beacons_info <- file.path(path_to_atlas_info, "Beacons_info.xlsx")
path_to_base_stations_info <- file.path(path_to_atlas_info, "Base_stations_info.csv")
time_of_bs_detections_summary_retrieval <- "2025-Jul-13 18:00"
min_detection_ratio <- 0.7   # minimum ratio between number of hours that a base station detected a certain beacon to the total number of hours
min_detections_number <- 300
max_distance_beacon_bs_km <- 7
### USER'S INPUT END

# Upload the hourly detections of each beacon by each base station
base_stations_summary_per_tag <- readRDS(file.path(folder_to_save_the_beacons_data, "base_stations_summary_per_beacon.Rds"))

# Upload the beacons info table
beacons_info <- read_excel(path_to_beacons_info)

# Upload the base stations info table
bs_info <- read.csv(path_to_base_stations_info)

# beacons_detection_ratio_per_bs <- generate_beacons_base_station_summary_table_over_time_periods(
#   beacons_info,
#   bs_info,
#   time_of_bs_detections_summary_retrieval,
#   base_stations_summary_per_tag,
#   min_detections_number)
# 
# saveRDS(beacons_detection_ratio_per_bs, file = file.path(folder_to_save_the_beacons_data, "beacons_detection_ratio_per_bs.Rds"))

beacons_detection_ratio_per_bs <- readRDS(file = file.path(folder_to_save_the_beacons_data, "beacons_detection_ratio_per_bs.Rds"))

beacons_detection_ratio_per_hour <- generate_bs_hour_detection_rate_table_from_beacons(
  base_stations_summary_per_tag, 
  beacons_detection_ratio_per_bs, 
  min_detection_ratio,
  max_distance_beacon_bs_km)

# print(beacons_detection_ratio_per_hour)

saveRDS(beacons_detection_ratio_per_hour, file.path(folder_to_save_the_beacons_data, "beacons_detection_ratio_per_hour.Rds"))
