# Prepare ATLAS data for ExMove

library("dplyr")

eminem_data <- readRDS("C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/Yehuda_jackals_data/eminem_filtered_tracks.rds")
kate_data <- readRDS("C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/Yehuda_jackals_data/kate_filtered_tracks.rds")

source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))
# Convert Unix timestamp to a UTC humandate in ATLAS format
eminem_data$DateTime <- unix_timestamp_to_human_date(eminem_data$TIME)
kate_data$DateTime <- unix_timestamp_to_human_date(kate_data$TIME)

# Rename columns to match the ExMove format
eminem_data <- eminem_data %>% rename(TagID = Tag)
# eminem_data <- eminem_data %>% rename(DateTime = dateTime)

kate_data <- kate_data %>% rename(TagID = Tag)
# kate_data <- kate_data %>% rename(DateTime = dateTime)

# Save the data as .csv
write.csv(eminem_data, "722_2023_GJ.csv", row.names = FALSE)
write.csv(kate_data, "727_2022_GJ.csv", row.names = FALSE)

# test commit
# # Prepare for ExMove- if the ATLAS data is already in the R environemt
# source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))
# # Convert Unix timestamp to a UTC humandate in ATLAS format
# raw_location_data$DateTime <- unix_timestamp_to_human_date(raw_location_data$TIME)
# raw_location_data <- raw_location_data %>% rename(TagID = TAG)
# write.csv(raw_location_data, "0426_202212_BO.csv", row.names = FALSE)
