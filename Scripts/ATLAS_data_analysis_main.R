# This script runs the entire routine to get the ATLAS data and filter them

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# Get the required paths from the config file config.R
source(file.path(getwd(), "Scripts", "config.R"))

tag_numbers = c(972006000837)
Start_Time_Str ='2023-12-24 00:00:01' # start time in UTC
End_Time_Str   ='2023-12-25 00:00:01' # end time in UTC

# tag_numbers = c(972006000837, 972006000944, 972006000544)
# Start_Time_Str ='2024-09-05 08:00:00' # start time in UTC
# End_Time_Str   ='2024-09-05 09:00:00' # end time in UTC

source(paste0(path_to_scripts,"install_required_R_packages.R"))

source(paste0(path_to_scripts,"get_ATLAS_data.R"))
#TODO Add a part that saves the data into SQLite and retrieved it from there

source(paste0(path_to_atlas_data_analysis_repo,"Track_cpp.R"))

# Calculate the confidence of each location point
raw_data_with_confidence_levels <- TrackConfidenceLevelcpp(RawLoc0,conectedVel=20,conectedDist=NA,stdlim=80,minNBSforConf2=7,minNBSforConf1=4,Nconf1forConf2=5)

# Filter all data with conf = 2
data__with_confidence_2 <- raw_data_with_confidence_levels %>%
  filter(Conf == 2)
