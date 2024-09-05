# This script runs the entire routine to get the ATLAS data and filter them

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# DECIDE LATER IF I WANT TO USE A CONFIG FILE
# path_to_config_file = "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Scripts/"
# source(paste0(path_to_config_file,"Config.R"))

# path of the data analysis repo ("Functions") in your computer
path_to_atlas_data_analysis_repo <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/"
path_to_scripts <- paste0(path_to_atlas_data_analysis_repo,"Scripts/")

tag_numbers = c(972006000837, 972006000944, 972006000544)

Start_Time_Str ='2024-09-05 00:08:00' # start time in UTC
End_Time_Str   ='2024-09-05 00:09:00' # end time in UTC

source(paste0(path_to_scripts,"install_required_R_packages.R"))

source(paste0(path_to_scripts,"get_ATLAS_data.R"))
#TODO Add a part that saves the data into SQLite and retrieved it from there

source(paste0(path_to_atlas_data_analysis_repo,"Track_cpp.R"))

# Calculate the confidence of each location point
confidence_vector <- TrackConfidanceLevelcpp(RawLoc0,conectedVel=20,conectedDist=NA,stdlim=80,minNBSforConf2=7,minNBSforConf1=4,Nconf1forConf2=5)
  

  
