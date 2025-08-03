# Configuration file for the atlasRF filter

### 1. Data to retrieve
species_id <- "BO"
tag_number <- 972006000856
start_time <- '2024-03-31 00:01:00'
end_time <- '2024-04-01 23:59:00'

### 1. Paths and folders

## 1.1. ATLAS data retrieval
fodler_path_to_retrieve_sqlite_files <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Outliers_characterization/Nesting_barn_owls/Nesting_BO_DB/Raw_data"

## 1.2. Feature metadata
base_stations_info_path <- "C:/Users/netat/Documents/Movement_Ecology/ATLAS/Base_stations_beacons_info/Base_stations_info.csv"
folder_of_beacons_info_tables <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Filter_development/Feature_engineering"
filename_beacons_detection_ratio_table <- "beacons_detection_ratio_per_hour.Rds"
filename_base_stations_summary_per_beacon <- "base_stations_summary_per_beacon.Rds"
folder_to_save_daa_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Outliers_characterization/Nesting_barn_owls/Nesting_BO_DB/Data_with_features"
path_to_save_feature_histograms <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Outliers_characterization/Nesting_barn_owls/Histograms_outlier_labels_threshold_5m"

## 1.3. Nesting Barn Owls data
path_to_nesting_bo_metadata <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Outliers_characterization/Nesting_barn_owls/Nesting_barn_owls.csv"
folder_to_save_labeled_data <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Outliers_characterization/Nesting_barn_owls/Nesting_BO_DB/Nesting_BO_Labeled_data"

### 2. ATLAS Data Retrieval Settings

# Define if to retrieve data from the ATLAS server (TRUE),
# or load data from an existing .sqlite file (FALSE)
retrieve_data_from_server <- FALSE

# Choose if to save or not to save new data retrieved from the ATLAS server as sqlite
# The code checks this variable only if retrieve_data_from_server == TRUE
save_data_to_sqlite_file <- TRUE
fodler_path_to_save_sqlite_files <- fodler_path_to_retrieve_sqlite_files

# Choose if to save or not to save new data retrieved from the ATLAS server as csv
# The code checks this variable only if retrieve_data_from_server == TRUE
save_data_to_csv_file <- FALSE
path_to_csv_files <- fodler_path_to_retrieve_sqlite_files

## Credentials of the Harod ATLAS database
system_name_harod <- "Harod"
# username 
db_username_harod <- 'roatlasharod2'            
# password
db_pass_harod <- 'harodATLASysro*24&10'
# host ip address
db_host_ip_harod <- '132.67.132.47' 
# port Number
db_port_number_harod <- 5900   
# name of data base
db_name_harod <- 'harod'

### 3. ATLAS time and coordinates information
atlas_time_format <- "%Y-%m-%d %H:%M:%S"
atlas_time_zone <- "UTC"
data_crs <- 2039

### 4. Feature Calculation Settings
# Threshold on the hourly number of detection of the beacons
low_beacon_detection_fraction <- 0.6
# Size of the time window around each location point for the window-based features
half_time_window_size_sec <- 25

### 5. Feature Settings
# Define the columns that are not considered as features and should be excluded from the training set
non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "geometry",
                              "X_mean", "Y_mean", "X_median", "Y_median")

### 6. Nesting Barn Owls Labeling Settings
# Threshold of location's distance from nest box to be considered as an outlier
threshold_dist_from_nest_m <- 5



