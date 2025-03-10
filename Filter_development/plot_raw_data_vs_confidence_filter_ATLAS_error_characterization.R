
source(paste0(getwd(),"/ATLAS_data_retrieval/retrieve_and_prepare_ATLAS_data.R"))
source(paste0(getwd(),"/Mapping_tools/interactive_map_single_atlas_dataset.R"))
source(paste0(getwd(),"/Mapping_tools/interactive_map_two_atlas_datasets.R"))

library(leaflet)
library(sf)
library(RColorBrewer)
library(htmltools)

# # Generate a map of a single data set
# map <- interactive_map_single_atlas_dataset(dd=raw_location_data)
# print(map)

# Generate a map of the raw data vs filtered data with confidence = 2
map <- interactive_map_two_atlas_datasets(dd1=raw_data_with_confidence_levels,
             dd2=data__with_confidence_2,
             MapProvider='Esri.WorldImagery',
             legendLabels=c("Raw Data", "Data with Confidence = 2"))

print(map)