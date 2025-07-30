library(ggplot2)

# Create the horizontal bar plot of the time range in each sqlite file
plot_data_time_ranges <- function(files_metadata, plot_resolution, species_id, path_to_species) {
  
  p <- ggplot(files_metadata, aes(x = Start_time, xend = End_time, y = as.factor(TAG), yend = as.factor(TAG), color = substr(TAG, nchar(TAG) - 3, nchar(TAG)))) +
    geom_segment(linewidth = 5) +  # Use geom_segment to create bars
    scale_x_datetime(labels = date_format("%Y-%m-%d"), breaks = date_breaks(plot_resolution)) +  # Customize X-axis to show date breaks
    labs(x = "Time", y = "Tag Number", title = paste(species_id, "data:", "Time Ranges by Tag Number")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")  # Rotate X-axis labels and remove legend if desired
  print(p)
  
  ggsave(filename = paste0(path_to_species, "/time_ranges_plot_", species_id, ".png"),
         plot = p, width = 10, height = 6, dpi = 300, bg = "white")
  
}
