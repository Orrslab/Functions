# Analyze the circular variance of the participating base stations of each localization

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(ggplot2)
library(data.table)

source(file.path(getwd(), "Build_atlasRF/Feature_engineering/load_data_with_features.R"))

# USER'S INPUT BEGIN

analysis_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Circular_variance_of_participating_base_stations"

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

# Subset only rows with non-missing circular variance
plot_data <- localization_data[!is.na(circ_variance)]

# Plot: circular variance distribution by Outliers status
p <- ggplot(plot_data, aes(x = circ_variance, fill = Outliers)) +
  geom_histogram(bins = 40, color = "black", alpha = 0.7) +
  facet_wrap(~ Outliers, ncol = 1) +
  scale_fill_manual(values = c("valid" = "forestgreen", "outlier" = "red")) +
  labs(
    title = "Distribution of Circular Variance by Outliers Label",
    x = "Circular Variance (1 - R̄)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

# Save the plot
file_name <- file.path(analysis_folder, "circular_variance_outliers_all_species.png")
ggsave(file_name, plot = p, width = 8, height = 6, bg = "white")

# Plot facet histograms of the circular varianve versus label- each species on a separate canvas
p <- 
  ggplot(localization_data[!is.na(circ_variance)], 
         aes(x = circ_variance, fill = Outliers)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 40) +
  facet_wrap(~ Species_id, scales = "free_y") +
  scale_fill_manual(values = c("valid" = "forestgreen", "outlier" = "red")) +
  labs(
    title = "Circular Variance versus Outliers label- All Species",
    x = "Circular Variance",
    y = "Frequency"
  ) +
  theme_minimal()

file_name <- paste0(analysis_folder, "/circular_variance_per_species", ".png")
ggsave(file_name, plot = p, width = 8, height = 6, bg = "white")

# # Generate the new feature (only if both components exist)
# localization_data[!is.na(circ_variance) & !is.na(Min_distance_to_BS), 
#                    combined_circ_dist_feature := circ_variance * log1p(Min_distance_to_BS)]
# 
# # Filter for rows where feature is available
# plot_data <- localization_data[!is.na(combined_circ_dist_feature)]
# 
# # Plot 1: Histogram by Outliers label
# p1 <- ggplot(plot_data, aes(x = combined_circ_dist_feature, fill = Outliers)) +
#   geom_histogram(bins = 40, color = "black", alpha = 0.7) +
#   facet_wrap(~ Outliers, ncol = 1) +
#   scale_fill_manual(values = c("valid" = "forestgreen", "outlier" = "red")) +
#   labs(
#     title = "Distribution of Combined Feature by Outliers Label",
#     x = "Combined Feature: circ_variance * log1p(MinDistanceToBS)",
#     y = "Frequency"
#   ) +
#   theme_minimal() +
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),
#     plot.title = element_text(size = 14, face = "bold"),
#     legend.position = "none"
#   )
# 
# ggsave(file.path(analysis_folder, "combined_feature_outliers_all_species.png"),
#        plot = p1, width = 8, height = 6, bg = "white")
# 
# 
# # Plot 2: Faceted by Species
# p2 <- ggplot(plot_data, aes(x = combined_circ_dist_feature, fill = Outliers)) +
#   geom_histogram(position = "identity", alpha = 0.5, bins = 40) +
#   facet_wrap(~ Species_id, scales = "free_y") +
#   scale_fill_manual(values = c("valid" = "forestgreen", "outlier" = "red")) +
#   labs(
#     title = "Combined Feature by Outliers Label - Faceted by Species",
#     x = "Combined Feature",
#     y = "Frequency"
#   ) +
#   theme_minimal()
# 
# ggsave(file.path(analysis_folder, "combined_feature_per_species.png"),
#        plot = p2, width = 8, height = 6, bg = "white")
