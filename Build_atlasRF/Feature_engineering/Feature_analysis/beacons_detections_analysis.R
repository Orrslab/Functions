library(ggplot2)
library(dplyr)
library(lubridate)
library(viridis)

### USER'S INPUT BEGIN
folder_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Beacons_data_analysis/Time_series_of_detections_per_base_station"
### USER'S INPUT END

# Format the time in HOUR column
df <- base_stations_summary_per_tag %>%
  mutate(HOUR = as.POSIXct(HOUR / 1000, origin = "1970-01-01", tz = "UTC"))

# Generate time series of the total detections by each base station from all the beacons- normalized be the number of beacons per hour

example_bs <- 972006024

df_bs_time <- df %>%
  filter(BS == example_bs) %>%
  group_by(HOUR) %>%
  summarise(
    total_detections = sum(DETECTIONS, na.rm = TRUE),
    unique_beacons = n_distinct(TAG[DETECTIONS > 0]),  # Count beacons with at least one detection
    normalized_detections = total_detections / unique_beacons
  )

p2 <- ggplot(df_bs_time, aes(x = HOUR, y = normalized_detections)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(title = paste("Normalized Detections of Base Station", example_bs, "Over Time"),
       x = "Hour", y = "Detections per Beacon")

print(p2)

# Save plot as PNG
ggsave(
  filename = file.path(folder_to_save_results, paste0("normalized_detections_bs_", example_bs, ".png")),
  plot = p2,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)

# df_heatmap <- df %>%
#   group_by(TAG, BS) %>%
#   summarise(total_detections = sum(DETECTIONS, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(
#     BS_short = substr(as.character(BS), nchar(as.character(BS)) - 1, nchar(as.character(BS)))
#   )
# 
# p <- ggplot(df_heatmap, aes(x = BS_short, y = as.factor(TAG), fill = total_detections)) +
#   geom_tile() +
#   scale_fill_viridis_c(trans = "log10") +
#   theme_minimal() +
#   labs(title = "Heatmap of Detections: TAG vs. Base Station Number",
#        x = "Base Station Number", y = "Tag Number", fill = "Detections (log10)")
# 
# 
# print(p)

