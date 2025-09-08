
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# Load config
source(file.path(getwd(), "Filter_development/config_build_atlasRF.R"))

# Load the other functions that do each steps of building the atlasRF
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/main_establish_labeled_database.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/main_calculate_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/main_clean_features_data.R"))

# Run the selected steps based on their flags from the config
if (config$workflow_steps_to_run$establish_labeled_db){
  main_establish_labeled_database(config)
}

if (config$workflow_steps_to_run$calculate_features) {
  main_calculate_features(config)
}

if (config$workflow_steps_to_run$clean_features_data) {
  main_clean_features_data(config)
}
