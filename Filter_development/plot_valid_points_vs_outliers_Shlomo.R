
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

library(leaflet)
library(sf)
library(RColorBrewer)
library(htmltools)

# # Generate a map of a single data set
# map <- atl_mapleaf(dd=raw_location_data)
# print(map)

# Insert the sqlite file name and full path
filename <- "All_data_Tag_691_HandFiltered"
path_to_shlomo_data <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Shlomo_Cain/"
sqlite_fullpath <- paste0(path_to_shlomo_data, filename, ".sqlite")

# Upload data from sqlite
source(paste0(getwd(), "load_atlas_data_from_sqlite.R"))
filtered_data <- load_atlas_data_from_sqlite(sqlite_filepath = sqlite_fullpath)

# Split the dataset into valid points and outliers
valid_points <- filtered_data[filtered_data$Outliers == 0, ]
outliers <- filtered_data[filtered_data$Outliers == 1, ]

source(paste0(getwd(),"/Mapping_tools/interactive_map_two_atlas_datasets.R"))
# Generate a map of the valid points vs outliers
map <- interactive_map_two_atlas_datasets(dd1=valid_points,
                                          dd2=outliers,
                                          MapProvider='Esri.WorldImagery',
                                          legendLabels=c("Valid Points", "Outliers"))

print(map)