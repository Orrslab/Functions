library(ggplot2)

#' Plot Time Ranges of Tag Data Files
#'
#' This function creates a horizontal bar plot showing the time ranges
#' (start and end times) of each tag's data within a species. The plot is
#' saved as a PNG file in the species folder.
#'
#' @param files_metadata A dataframe containing metadata for the tag files.
#'   Must include the columns: \code{Start_time}, \code{End_time}, and \code{TAG}.
#' @param plot_resolution Character. Interval for breaks on the x-axis (e.g., 
#'   \code{"1 month"}, \code{"1 week"}, \code{"3 days"}). Passed to 
#'   \code{scales::date_breaks()}.
#' @param species_id Character. Identifier of the species (used in plot title and filename).
#' @param path_to_species Character. Path to the species folder where the plot
#'   will be saved.
#'
#' @return This function does not return a value. It produces and saves a PNG plot
#'   named \code{time_ranges_plot_<species_id>.png} in the given species folder.
#'
#' @details
#' The function uses \pkg{ggplot2} to create a horizontal bar plot with 
#' \code{Start_time} and \code{End_time} on the x-axis and tag IDs on the y-axis.
#' Colors are automatically assigned based on the last 4 characters of the tag ID.
#'
#' @import ggplot2
#' @importFrom scales date_format date_breaks
#' @export
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
