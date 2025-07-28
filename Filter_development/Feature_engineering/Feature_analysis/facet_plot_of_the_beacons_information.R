# facet plot of the beacons information

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(ggplot2)
library(data.table)

source(file.path(getwd(), "Filter_development/Feature_engineering/load_data_with_features.R"))

# USER'S INPUT BEGIN

analysis_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Beacons_data_analysis"

# tables_to_load <- c("LOCALIZATIONS", 
#                     "DETECTIONS", 
#                     "PARTICIPATING_BASE_STATIONS", 
#                     "MISSED_BASE_STATIONS", 
#                     "PROPERTIES")

tables_to_load <- c("LOCALIZATIONS")

# USER'S INPUT END

# Load the features data
features_data <- load_data_with_features(tables_to_load)

localization_data <- features_data$LOCALIZATIONS

# Ensure data is a data.table
localization_data <- as.data.table(localization_data)

# Ensure Outliers is a factor
localization_data[, Outliers := as.factor(Outliers)]

# Subset only rows with non-missing feature values
plot_data <- localization_data[!is.na(detection_probability_factor_3_closest_bs)]

# Plot: feature distribution by Outliers status
p <- ggplot(plot_data, aes(x = detection_probability_factor_3_closest_bs, fill = Outliers)) +
  geom_histogram(bins = 40, color = "black", alpha = 0.7) +
  facet_wrap(~ Outliers, ncol = 1) +
  scale_fill_manual(values = c("valid" = "forestgreen", "outlier" = "red")) +
  labs(
    title = "Distribution of detection_probability_factor_3_closest_bs by Outliers Label",
    x = "detection_probability_factor_3_closest_bs",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

# Save the plot
file_name <- file.path(analysis_folder, "detection_probability_factor_3_closest_bs_all_species.png")
ggsave(file_name, plot = p, width = 8, height = 6, bg = "white")

#################################################

# Subset only rows with non-missing feature values
plot_data <- localization_data[!is.na(mean_max_snr_mean_ratio_3_closest_bs)]

# Plot: feature distribution by Outliers status
p <- ggplot(plot_data, aes(x = mean_max_snr_mean_ratio_3_closest_bs, fill = Outliers)) +
  geom_histogram(bins = 40, color = "black", alpha = 0.7) +
  facet_wrap(~ Outliers, ncol = 1) +
  scale_fill_manual(values = c("valid" = "forestgreen", "outlier" = "red")) +
  labs(
    title = "Distribution of mean_max_snr_mean_ratio_3_closest_bs by Outliers Label",
    x = "mean_max_snr_mean_ratio_3_closest_bs",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

# Save the plot
file_name <- file.path(analysis_folder, "mean_max_snr_mean_ratio_3_closest_bs_all_species.png")
ggsave(file_name, plot = p, width = 8, height = 6, bg = "white")

#################################################

# Subset only rows with non-missing feature values
plot_data <- localization_data[!is.na(num_low_detection_beacons_by_participating_bs_3_closest_bs)]

# Plot: feature distribution by Outliers status
p <- ggplot(plot_data, aes(x = num_low_detection_beacons_by_participating_bs_3_closest_bs, fill = Outliers)) +
  geom_histogram(bins = 40, color = "black", alpha = 0.7) +
  facet_wrap(~ Outliers, ncol = 1) +
  scale_fill_manual(values = c("valid" = "forestgreen", "outlier" = "red")) +
  labs(
    title = "Distribution of num_low_detection_beacons_by_participating_bs_3_closest_bs by Outliers Label",
    x = "num_low_detection_beacons_by_participating_bs_3_closest_bs",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

# Save the plot
file_name <- file.path(analysis_folder, "num_low_detection_beacons_by_participating_bs_3_closest_bs_all_species.png")
ggsave(file_name, plot = p, width = 8, height = 6, bg = "white")