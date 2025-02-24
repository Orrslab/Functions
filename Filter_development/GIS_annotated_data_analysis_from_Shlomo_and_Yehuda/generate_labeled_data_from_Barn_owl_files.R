# This script uploads labeleld outliers from a file, 
# retrieves the raw data of the corresponding time range from the ATLAS, 
# combines the Outliers and Valid Points to one dataframe with an Outliers column,
# and saves the combined dataframe as sqlite. 

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

source(file.path(getwd(), "config.R"))

filename <- "raw685_fromR"
path_to_shlomo_data <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Shlomo_Cain/Tag_685_not_filtered/"
fullpath <- paste0(path_to_shlomo_data, filename, ".csv")

# Load the outliers data from csv
only_outliers <- read.csv(fullpath)

# # Plot the data on a map
# source("C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Mapping_tools/interactive_map_single_atlas_dataset.R")
# map <- interactive_map_single_atlas_dataset(dd=only_outliers)
# print(map)

data_request <- list(
  list(tag = 972006000685, 
       start_time = '2022-05-01 18:00:00', 
       end_time = '2022-05-02 05:00:00')
)

# Get the ATLAS data from the server
source(paste0(path_to_atlas_data_analysis_repo,"get_ATLAS_data.R"))
raw_location_data = get_ATLAS_data(data_requests = data_request, 
                                   retrieve_data_from_server = TRUE,
                                   save_data_to_sqlite_file = FALSE,
                                   full_paths_to_sqlite_files = fullpath_to_sqlite_file)

# Add an Outliers columns and set all the rows to zero
raw_location_data$Outliers <- 1

# Loop through each TIME value in the outliers data frame
for (time_val in only_outliers$TIME) {
  # Check if the TIME value exists in raw_location_data
  match_idx <- which(raw_location_data$TIME == time_val)
  
  if (length(match_idx) > 0) {
    # If found, set Outliers column to 1
    raw_location_data$Outliers[match_idx] <- 0
  } else {
    # If not found, print an error message with the TIME value
    cat("Error: TIME value", time_val, "not found in raw_location_data\n")
  }
}

# Save the data frame with the valid points and outliers into sqlite
source(paste0(path_to_atlas_data_analysis_repo, "save_ATLAS_data_to_sqlite.R"))
sqlite_fullpath <- paste0(path_to_shlomo_data, filename, ".sqlite")
save_ATLAS_data_to_sqlite(localizations_data = raw_location_data, fullpath = sqlite_fullpath)

