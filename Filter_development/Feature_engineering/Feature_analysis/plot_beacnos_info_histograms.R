library(ggplot2)

### USER'S INPUT BEGIN
folder_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Beacons_data_analysis/Normalized_detectiona_multiplication_per_base_station"
### USER'S INPUT END

p <- ggplot(beacons_detection_ratio_per_hour, aes(x = BEACONS_DETECTION_MULTIPLICATION)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  labs(
    title = "Histogram of BEACONS_DETECTION_MULTIPLICATION",
    x = "multiplication of the normalized detections of each beacon per base station and hour",
    y = "Count"
  ) +
  theme_minimal()

print(p)

# Get unique base station numbers
unique_bs <- unique(beacons_detection_ratio_per_hour$BS)

# Loop over each base station and generate a histogram
for (bs in unique_bs) {
  df_bs <- beacons_detection_ratio_per_hour %>%
    filter(BS == bs)
  
  p <- ggplot(df_bs, aes(x = BEACONS_DETECTION_MULTIPLICATION)) +
    geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
    labs(
      title = paste("Histogram of BEACONS_DETECTION_MULTIPLICATION for BS", bs),
      x = "Multiplication of normalized detections per hour",
      y = "Count"
    ) +
    theme_minimal()
  
  # Save plot to PNG
  ggsave(filename = file.path(folder_to_save_results, paste0("BS_", bs, "_histogram.png")),
         plot = p,
         width = 8, height = 5, dpi = 300, bg = "white")
}