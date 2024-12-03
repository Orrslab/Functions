
# Get the required paths from the config file config.R
source(file.path(getwd(), "config.R"))

source(paste0(path_to_scripts,"ATLAS_data_analysis_main.R"))
source(paste0(path_to_atlas_maps,"interactive_maps.R"))

library(leaflet)
library(sf)
library(RColorBrewer)
library(htmltools)

# Generate a map of a single data set
map <- atl_mapleaf(dd=raw_location_data)
print(map)

# # Generate a map of the raw data vs filtered data with confidence = 2
# map <- atl_mapleaf2(dd1=raw_data_with_confidence_levels,
#              dd2=data__with_confidence_2,
#              MapProvider='Esri.WorldImagery',
#              legendLabels=c("Raw Data", "Data with Confidence = 2")) 
#   
# print(map)