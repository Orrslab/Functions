# main script for RF model development for outliers filtering in the ATLAS data

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(caret)
library(ranger)
library(ggplot2)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/split_training_test_data_stratified_by_Outliers.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/split_training_test_data_stratified_by_time.R"))

### USER INPUT BEGIN
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
features_data_filename <- "Features_data_for_RF_all_species.sqlite"

# Which fraction of the data you want to use for training of the random forest model?
training_data_fraction <- 0.8

# Remove Stops flag
remove_stops_from_training_set <- FALSE

# Define the columns that are not considered as features and should be excluded from the training set
non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "dateTime", "DAY", "geometry",
                              "Species_id", "X_mean", "Y_mean", "X_median", "Y_median", "Is_stop")

# Folder to save the model results
RF_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/GJ_29-07-25"

training_set_filename <- "training_set.rds"
test_set_filename <- "test_set.rds"

### USER INPUT END

features_filepath <- file.path(path_to_data_with_features, features_data_filename)

# Load the features data from sqlite
features_data <- load_tables_from_sqlite_file(
  sqlite_filepath = features_filepath,
  tables = "LOCALIZATIONS")

localization_data <- features_data$LOCALIZATIONS

#### EXTRACT THE DATA OF ONLY THE JACKALS
localization_data <- localization_data[localization_data$Species_id == "GJ", ]
####

# Split the data into a training set and a test set- stratify by Outliers
data_sets <- split_training_test_data_stratified_by_time(
  data = localization_data,
  train_frac = training_data_fraction)

# # Split the data into a training set and a test set- stratify by TIME
# data_sets <- split_training_test_temporal_stratified_species_stratified_by_time(
#   data = localization_data,
#   train_frac = training_data_fraction)

training_set <- data_sets$training_set
test_set <- data_sets$test_set

# Remove stops from the training set
if (remove_stops_from_training_set) {
  training_set <- training_set[training_set$Is_stop != 1, ]
}

# Remove the non-feature columns from the training set
training_set <- training_set %>%
  dplyr::select(-all_of(non_feature_column_names))

# save the datasets
saveRDS(training_set, file = file.path(RF_results_folder, training_set_filename))
saveRDS(test_set, file = file.path(RF_results_folder, test_set_filename))