
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/AUC_analysis/calculate_auc_per_feature.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/AUC_analysis/calculate_auc_per_feature_per_species.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/AUC_analysis/plot_auc_matrix_per_feature_per_species.R"))

# USER INPUT BEGIN
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
features_data_filename <- "Features_data_for_RF_all_species.sqlite"
auc_analysis_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/AUC_analysis"
auc_per_feature_filename <- "auc_per_feature.csv"
auc_per_feature_per_species_filename <- "auc_per_feature_per_species.csv"
# USER INPUT END

features_filepath <- file.path(path_to_data_with_features, features_data_filename)

# Load the features data from sqlite
data <- load_tables_from_sqlite_file(
  sqlite_filepath = features_filepath,
  tables = "LOCALIZATIONS")

localization_data <- data$LOCALIZATIONS

# Remove the TIME column
localization_data <- localization_data %>% select(-TIME)
# Remove the TAG column
localization_data <- localization_data %>% select(-TAG)

# Calculate the AUC value per feature
auc_per_feature <- calculate_auc_per_feature(df = localization_data)

# Order the features in ascending order
setorder(auc_per_feature, AUC)

# Save the AUC per feature as csv
fwrite(auc_per_feature, file.path(auc_analysis_folder, auc_per_feature_filename))

# Calculate the AUC value of each feature per species
auc_per_feature_per_species <- calculate_auc_per_feature_per_species(df = localization_data)

# Save the AUC per feature per species as csv
fwrite(auc_per_feature_per_species, file.path(auc_analysis_folder, auc_per_feature_per_species_filename))

# Calculate the AUC per feature and per species
auc_per_feature_per_species <- fread(file.path(auc_analysis_folder, auc_per_feature_per_species_filename))

# Plot a heat matrix of the AUC values
auc_plot <- plot_auc_matrix_per_feature_per_species(auc_per_feature_per_species)
print(auc_plot)

# Save the plot
ggsave(file.path(auc_analysis_folder, "auc_per_feature_per_species_heatmap.png"), plot = auc_plot, width = 10, height = 8, dpi = 300, bg = "white")
