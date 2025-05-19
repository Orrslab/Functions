library(ggplot2)
library(dplyr)
library(tidyr)

# Insert the full path of the csv with the elevation data you want to test
csv_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features/csv_files"
filename <- "CB_features_eng"

csv_path <- file.path(csv_folder, paste0(filename, ".csv"))

localization_data <- read.csv(csv_path)

elevation_values <- localization_data$DEM_elevation

print(paste0("Elevation range: ", range(elevation_values)))

summary(elevation_values)

# Plot the Elevation values over TIME
plot(localization_data$TIME, elevation_values, type = "l")

# Plot the evelation values versus Outliers values
boxplot(DEM_elevation ~ Outliers, data = localization_data,
        main = "Elevation by Outlier Category",
        xlab = "Outliers", ylab = "DEM_elevation",
        names = c("0", "1", "2"))

# Histograms of the Elevation values for each Outliers cathegory
ggplot(localization_data, aes(x = DEM_elevation)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ Outliers, scales = "free_y") +
  labs(
    title = "Elevation Distribution by Outlier Category",
    x = "Elevation",
    y = "Count"
  ) +
  theme_minimal()

# Histogram of the elevation values
hist(elevation_values, breaks = 50)

# Check for abnormal values
print(paste0("Abnormal elevation values: ", any(elevation_values < -500 | elevation_values > 1000)))
