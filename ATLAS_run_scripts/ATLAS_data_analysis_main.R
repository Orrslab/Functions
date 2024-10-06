# This script runs the entire routine to get the ATLAS data and filter them

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# Get the required paths from the config file config.R
source(file.path(getwd(), "config.R"))

source(paste0(path_to_scripts,"install_required_R_packages.R"))

source(paste0(path_to_scripts, "ATLAS_data_requests.R"))

source(paste0(path_to_atlas_data_analysis_repo,"get_ATLAS_data.R"))
raw_location_data = get_ATLAS_data()

source(paste0(path_to_atlas_data_analysis_repo,"Track_cpp.R"))

# Calculate the confidence of each location point
raw_data_with_confidence_levels <- TrackConfidenceLevelcpp(raw_location_data,
                                                           conectedVel=20,
                                                           conectedDist=NA,
                                                           stdlim=80,
                                                           minNBSforConf2=7,
                                                           minNBSforConf1=4,
                                                           Nconf1forConf2=5)

# Filter all data with conf = 2
data__with_confidence_2 <- raw_data_with_confidence_levels %>%
  filter(Conf == 2)
