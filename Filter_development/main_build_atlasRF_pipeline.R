
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
source(file.path(getwd(), "Filter_development/Random_Forest/main_generate_training_and_test_sets.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/main_tune_hyper_parameters_of_atlasRF.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/main_train_atlasRF.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/main_run_atlasRF_on_test_set.R"))

## Run the selected steps based on their flags from the config

# Establish a Labeled Database
if (config$workflow_steps_to_run$establish_labeled_db){
  main_establish_labeled_database(config)
}

# Calculate Features
if (config$workflow_steps_to_run$calculate_features) {
  main_calculate_features(config)
}

# Clean the Feature Data
if (config$workflow_steps_to_run$clean_features_data) {
  main_clean_features_data(config)
}

# Split the Cleaned Data into Training and Test Sets
if (config$workflow_steps_to_run$split_data_to_training_and_test) {
  main_generate_training_and_test_sets(config)
}

# Tune the atlasRF Hyperparameters
if (config$workflow_steps_to_run$tune_hyper_parameters) {
  main_tune_hyper_parameters_of_atlasRF(config)
}

# Train the atlasRF Filter
if (config$workflow_steps_to_run$train_atlasRF) {
  main_train_atlasRF(config)
}

# Evaluate atlasRF on the Test Set
if (config$workflow_steps_to_run$run_atlasRF_on_test_set) {
  main_run_atlasRF_on_test_set(config)
}

