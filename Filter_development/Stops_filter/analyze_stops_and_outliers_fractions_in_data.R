library(dplyr)

path_to_save_stops_analysis_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Stops_Filter/Stops_analysis"

# # Create a summary table for all the species
# summary_table <- table(localization_data$Is_stop, localization_data$Outliers)
# 
# # Print the table with clear labels- all species
# cat("Summary of Is_stop vs Outliers:\n")
# rownames(summary_table) <- c("Is_stop = 0", "Is_stop = 1")
# colnames(summary_table) <- c("Outliers = 0", "Outliers = 1")
# print(summary_table)

# Create the summary table- per species
summary_table <- localization_data %>%
  group_by(Species_id, Is_stop, Outliers) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Species_id, Is_stop) %>%
  mutate(Percentage = round(100 * Count / sum(Count), 2)) %>%
  ungroup() %>%
  arrange(Species_id, Is_stop, Outliers)

# Save the table as CSV
output_path <- file.path(path_to_save_stops_analysis_results, 
                         "outliers_distribution_per_stops_and_non_stops_per_species.csv")
write.csv(summary_table, output_path, row.names = FALSE)

# # Print the result
# print(summary_table)

# Calculate total points and number of stops per species
stop_percent_table <- localization_data %>%
  group_by(Species_id) %>%
  summarise(
    Total_points = n(),
    Num_stops = sum(Is_stop == 1, na.rm = TRUE),
    Stop_percentage = round(100 * Num_stops / Total_points, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(Stop_percentage))

output_path <- file.path(path_to_save_stops_analysis_results,
                         "percentage_of_stops_per_species.csv")

write.csv(stop_percent_table, output_path, row.names = FALSE)
