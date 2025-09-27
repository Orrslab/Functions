
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

# Load the config file
source(file.path(getwd(), "Apply_atlasRF/config_apply_atlasRF.R"))

# Load the other functions that do each step of the atlasRF filter
source(file.path(getwd(), "Apply_atlasRF/wrapper_retrieve_raw_atlas_data.R"))

###

# # Install the required R packages- if not yet installed
# source(file.path(getwd(),""ATLAS_data_retrieval/install_required_R_packages.R"))

# load unfiltered data from the ATLAS database
wrapper_retrieve_raw_atlas_data(config)
