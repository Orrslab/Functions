
### clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
###

### Set paths
fullpath_to_data <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Shlomo_Cain/labeled_data_with_features.sqlite"
path_to_functions <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/"
path_to_roc_analysis <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/ATLAS_run_scripts/ConfidenceFilterDevelopment/"

### Load the data
source(paste0(path_to_functions, "load_atlas_data_from_sqlite.R"))
location_data_labeled <- load_atlas_data_from_sqlite(fullpath_to_data)

# ROC analysis
source(paste0(path_to_roc_analysis, "evaluate_roc_parameters.R"))
speed_roc_parameters <- evaluate_roc_parameters(feature_data = location_data_labeled$Speed_m_s,
                                                outliers_data = location_data_labeled$Outliers)

std_roc_parameters <- evaluate_roc_parameters(feature_data = location_data_labeled$STD,
                                              outliers_data = location_data_labeled$Outliers)

nbs_roc_parameters <- evaluate_roc_parameters(feature_data = location_data_labeled$NBS,
                                              outliers_data = location_data_labeled$Outliers)

# # Plot the ROC curve of a single feature
# roc_parameters <- speed_roc_parameters
# plot(roc_parameters$fpr, roc_parameters$tpr, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve")

# Plot all roc curves on the same figure
# Plot the first ROC curve (Speed)
plot(speed_roc_parameters$fpr, speed_roc_parameters$tpr, type = "l", col = "blue",
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     main = "ROC Curve for Multiple Features", lwd = 2)

# Add the second ROC curve (STD)
lines(std_roc_parameters$fpr, std_roc_parameters$tpr, col = "red", lwd = 2)

# Add the third ROC curve (Number of Base Stations - NBS)
lines(nbs_roc_parameters$fpr, nbs_roc_parameters$tpr, col = "green", lwd = 2)

# Add a diagonal dashed line (random classifier)
abline(a = 0, b = 1, lty = 2, col = "black")

# Add a legend to differentiate the curves
legend("bottomright", legend = c("Speed", "STD", "NBS"),
       col = c("blue", "red", "green"), lwd = 2,
       cex = 0.8, # Makes the legend smaller
       bty = "n") # Removes the legend box for a cleaner look




