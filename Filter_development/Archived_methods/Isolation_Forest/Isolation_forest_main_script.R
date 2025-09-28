
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(isotree)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))

### USER'S INPUT BEGIN
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
features_data_filename <- "Features_data_for_RF_all_species.sqlite"

# Define the columns that are not considered as features and should be excluded from the training set
non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "dateTime", "DAY", "geometry",
                              "Species_id", "X_mean", "Y_mean", "X_median", "Y_median", "Is_stop",
                              "closest_missed_bs_distance", "mean_missed_bs_distance")

# Folder to save the model results
isoforest_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Isolation_Forest_Model/BO_30-07-25"

species_code <- "BO"

# Isolation Forest parameters
num_of_trees <- 100
sample_size_to_build_each_tree <- 256
votes_threshold <- 0.6
### USER'S INPUT END

features_filepath <- file.path(path_to_data_with_features, features_data_filename)

# Load the features data from sqlite
features_data <- load_tables_from_sqlite_file(
  sqlite_filepath = features_filepath,
  tables = "LOCALIZATIONS")

localization_data <- features_data$LOCALIZATIONS

#### Extract the data of only one species
localization_data <- localization_data[localization_data$Species_id == species_code, ]
####

# Remove the non-feature and non-numberi columns
localization_data_only_features <- localization_data %>%
  dplyr::select(-all_of(non_feature_column_names))

# Train the Isolation Forest model
# You can adjust ntrees (number of trees) and sample_size if needed
iso_model <- isolation.forest(localization_data_only_features, ntrees = num_of_trees, sample_size = sample_size_to_build_each_tree)

# Get anomaly scores
scores <- predict(iso_model, localization_data_only_features, type = "score")  # values between 0 and 1

# Set a threshold
predicted_outliers <- ifelse(scores > votes_threshold, 1, 0)

# # Step 6: Optional evaluation (if you have a labeled column)
# # Assume you have a binary column called "Is_outlier" (1 = true outlier)
# if ("Is_outlier" %in% colnames(barn_owl_data)) {
#   table(Prediction = predicted_outliers, Reference = barn_owl_data$Is_outlier)
# }