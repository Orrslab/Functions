
# Get the required paths from the config file config.R
source(file.path(getwd(), "config.R"))

# source(paste0(path_to_scripts,"ATLAS_data_analysis_main.R"))
source(paste0(path_to_atlas_maps,"interactive_maps.R"))

library(leaflet)
library(sf)
library(RColorBrewer)
library(htmltools)

# # Generate a map of a single data set
# map <- atl_mapleaf(dd=raw_location_data)
# print(map)

# Upload data from CSV
path_yehuda <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Yehuda_Samuel/"

raw_data_path <- paste0(path_yehuda, "eminem_night20.csv") 
filtered_data_path <- paste0(path_yehuda, "eminem_wrong_locs_night20.csv")

raw_data <- read.csv(raw_data_path)
filtered_data <- read.csv(filtered_data_path)

# Change column names to fit to the data format required for the mapping function
colnames(raw_data)[colnames(raw_data) == "Tag"] <- "TAG"
colnames(filtered_data)[colnames(filtered_data) == "Tag"] <- "TAG"

# Generate a map of the raw data vs filtered data with confidence = 2
map <- atl_mapleaf2(dd1=raw_data,
             dd2=filtered_data,
             MapProvider='Esri.WorldImagery',
             legendLabels=c("Raw Data", "Filtered Data"))

print(map)