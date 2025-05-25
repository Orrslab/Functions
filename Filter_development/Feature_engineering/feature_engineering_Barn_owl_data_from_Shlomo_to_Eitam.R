# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

### Set paths
data_folder <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Shlomo_Cain"
path_to_functions <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/"
path_to_roc_analysis <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/ATLAS_run_scripts/ConfidenceFilterDevelopment/"

### Load the raw labeled data
# Get a list of the fullpaths of all the sqlite files in the animal's data folder
source(paste0(path_to_functions, "get_full_paths_of_files_with_specified_extension.R"))
sqlite_full_paths <- get_full_paths_of_files_with_specified_extension(data_folder, "sqlite")

### Load the location data from all the files into one data frame
source(paste0(path_to_functions, "load_atlas_data_from_multiple_sqlite_files.R"))
location_data_labeled <- load_atlas_data_from_multiple_sqlite_files(sqlite_full_paths)

### Feature Engineering
source(paste0(path_to_functions, "atlas_metrics.R"))

# Time defference between consecutive points [s]
time_diff_s <- calculate_time_diff(location_data_labeled$TIME)

# Distance between consecutive points [m]
distance_m <- calculate_distance(location_data_labeled$X, location_data_labeled$Y)

# Speed [m/s]
location_data_labeled$Speed_m_s <- calculate_speed(distance_m, time_diff_s)

# Standard Deviation
location_data_labeled$STD <- calculate_std(location_data_labeled$VARX, 
                                           location_data_labeled$VARY, 
                                           location_data_labeled$COVXY)

# Cosine of the turning angle
location_data_labeled$cos_turning_angle <- calculate_cosine_turning_angle(X_column = location_data_labeled$X,
                                                                          Y_column = location_data_labeled$Y)

### Save into sqlite the labeled data with all the added features
source(paste0(path_to_functions, "save_ATLAS_data_to_sqlite.R"))
fullpath <- paste0(data_folder, "/labeled_data_with_features.sqlite")
save_ATLAS_data_to_sqlite(localizations_data=location_data_labeled, 
                          fullpath = fullpath)


### Data Statistics and Visualization
column_name <- "cos_turning_angle"
feature_data = location_data_labeled[[column_name]]
feature_name <- "cos(turning_angle)"
feature_units <- ""

# 1. Outliers proportion
table(location_data_labeled$Outliers)

# 2. Summary of the data
print(summary(feature_data))

# 3. Histogram
hist(feature_data, breaks = 100, main = paste(feature_name, "Distribution"), xlab = paste(feature_name, feature_units))

# 4. Logarithmic transformation of the feature values
log_feature <- log(feature_data + 1)  # Adding 1 to avoid log(0)
hist(log_feature, main = paste("Log-Transformed", feature_name, "Distribution"), xlab = paste0("Log(", feature_name, ")"), col = "lightblue", breaks = 30)

# 5. Box Plot
# Linear scale
boxplot(feature_data, main = paste(feature_name, "Quartiles"), ylab = paste(feature_name, feature_units), col = "lightblue")
# Logarithmic scale
boxplot(log_feature, main = paste("Logarithmic", feature_name, "Quartiles"), ylab = paste0("Log(", feature_name, ")"), col = "lightblue")

# 6. Density Plot
# Linear scale
plot(density(feature_data, na.rm = TRUE), main = paste(feature_name, "Density Plot"), xlab = paste(feature_name, feature_units), col = "blue")
# Logarithmic scale
plot(density(log_feature, na.rm = TRUE), main = paste0("Log(", feature_name, ") Density Plot"), xlab = paste0("Log(", feature_name, ")"), col = "blue")

# 7. Facet Plot- outliers versus non-outliers
library(ggplot2)

# Linear scale
ggplot(location_data_labeled, aes(x = !!sym(column_name))) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Outliers) +
  labs(title = paste(feature_name, "Distribution by Outlier Status"), x = paste(feature_name, feature_units), y = "Frequency") +
  theme_minimal()

# Features with integer values, such as NBS
ggplot(location_data_labeled, aes(x = !!sym(column_name))) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Outliers) +
  labs(title = paste(feature_name, "Distribution by Outlier Status"), 
       x = paste(feature_name, feature_units), 
       y = "Frequency") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Adjusts breaks to show only integer values
  theme_minimal()

# Logarithmic scale
ggplot(location_data_labeled, aes(x = log_feature)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Outliers) +
  labs(title = paste(feature_name, "Log-Transformed Distribution by Outlier Status"), 
       x = paste("Log of", feature_name, feature_units), 
       y = "Frequency") +
  theme_minimal()
