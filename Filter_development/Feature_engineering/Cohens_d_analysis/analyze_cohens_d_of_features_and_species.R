
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/Cohens_d_analysis/calculate_cohens_d_per_feature.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/Cohens_d_analysis/calculate_cohens_d_per_feature_per_species.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/Cohens_d_analysis/plot_cohens_d_heatmap_per_species.R"))

# USER INPUT BEGIN
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
features_data_filename <- "Features_data_for_RF_all_species.sqlite"
cohens_d_analysis_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Cohens_d_analysis"
cohens_d_per_feature_filename <- "cohens_d_per_feature.csv"
cohens_d_per_feature_per_species_filename <- "cohens_d_per_feature_per_species.csv"
# USER INPUT END

features_filepath <- file.path(path_to_data_with_features, features_data_filename)

# Load the features data from sqlite
data <- load_tables_from_sqlite_file(
  sqlite_filepath = features_filepath,
  tables = "LOCALIZATIONS")

localization_data <- data$LOCALIZATIONS

# Ensure Outliers is a factor and define level order
localization_data$Outliers <- factor(localization_data$Outliers, levels = c("valid", "outlier"))

# Print the order of levels of Outliers in order to interpret correctly the results of Cohen's d
print(paste0("Level of the Outleirs column are: ", levels(localization_data$Outliers)))

# Remove the TIME column
localization_data <- localization_data %>% select(-TIME)
# Remove the TAG column
localization_data <- localization_data %>% select(-TAG)

# Calculate Cohen's d per feature
cohens_d_per_feature <- calculate_cohens_d_per_feature(df = localization_data)

# Save the cohen's d per feature as csv
fwrite(cohens_d_per_feature, file.path(cohens_d_analysis_folder, cohens_d_per_feature_filename))

# Calculate Cohen's d per feature and per species
cohens_d_per_feature_per_species <- calculate_cohens_d_per_feature_per_species(df = localization_data)

# Save the Cohen's d per feature and per species as csv
fwrite(cohens_d_per_feature_per_species, file.path(cohens_d_analysis_folder, cohens_d_per_feature_per_species_filename))

cohens_d_plot <- plot_cohens_d_heatmap_per_species(cohens_d_df = cohens_d_per_feature_per_species)
print(cohens_d_plot)

# Save the plot
ggsave(file.path(cohens_d_analysis_folder, "cohens_d_per_feature_per_species_heatmap.png"), plot = cohens_d_plot, width = 10, height = 8, dpi = 300, bg = "white")
