library(gt)
library(dplyr)
library(scales)

### USER'S INPUT BEGIN
folder_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Beacons_data_analysis"
### USER'S INPUT END

# Create the summary table with normalized detections
table_to_display <- beacons_detection_ratio_per_bs %>%
  select(TAG, BS, OVERLAP_START, OVERLAP_END, NORMALIZED_DETECTIONS)

gtsave(
  table_to_display %>%
    gt() %>%
    data_color(
      columns = NORMALIZED_DETECTIONS,
      colors = scales::col_numeric(
        palette = viridis::viridis(20),
        domain = range(table_to_display$NORMALIZED_DETECTIONS, na.rm = TRUE)
      )
    ) %>%
    fmt_number(columns = NORMALIZED_DETECTIONS, decimals = 3) %>%
    tab_header(
      title = "Normalized Detections per Beacon–Base Station Pair and Time Period"
    ) %>%
    cols_label(
      TAG = "Beacon Number",
      BS = "Base Station Number",
      OVERLAP_START = "Start Time",
      OVERLAP_END = "End Time",
      NORMALIZED_DETECTIONS = "Normalized Detections"
    ),
  filename = file.path(folder_to_save_results, "normalized_detections_table.png")
)

# Loop over unique TAGs and save individual tables
unique_tags <- unique(table_to_display$TAG)

for (tag in unique_tags) {
  tag_table <- table_to_display %>% 
    filter(TAG == tag)
  
  tag_gt <- tag_table %>%
    gt() %>%
    data_color(
      columns = NORMALIZED_DETECTIONS,
      colors = scales::col_numeric(
        palette = viridis::viridis(20),
        domain = range(table_to_display$NORMALIZED_DETECTIONS, na.rm = TRUE)
      )
    ) %>%
    fmt_number(columns = NORMALIZED_DETECTIONS, decimals = 3) %>%
    tab_header(
      title = paste("Normalized Detections - Beacon", tag)
    ) %>%
    cols_label(
      TAG = "Beacon Number",
      BS = "Base Station Number",
      OVERLAP_START = "Start Time",
      OVERLAP_END = "End Time",
      NORMALIZED_DETECTIONS = "Normalized Detections"
    )
  
  # Save each table as a separate PNG
  gtsave(
    tag_gt,
    filename = file.path(folder_to_save_results, paste0("normalized_detections_beacon_", tag, ".png"))
  )
}