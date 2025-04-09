
plot_exploratory_analysis <- function(species_data,
                                      feature_column_name,
                                      feature_name,
                                      feature_units) {
  
  # Filter out non-finite values before plotting
  species_data_clean <- species_data[!is.na(species_data[[feature_column_name]]) & is.finite(species_data[[feature_column_name]]), ]
  feature_data = species_data_clean[[feature_column_name]]
  
  # Summary of the data
  feature_summary <- capture.output(summary(feature_data))
  summary_df <- data.frame(Summary = feature_summary)
  summary_filename <- paste0(feature_column_name, "_summary.csv")
  write.csv(summary_df, file = file.path(path_to_save_results_species, summary_filename), row.names = FALSE)
  
  # Histogram
  num_bins <- min(100, max(10, length(feature_data) / 10))  # Adjust bins based on data size
  hist_filename <- paste0(feature_column_name, "_histogram.png")
  png(file.path(path_to_save_results_species, hist_filename), width = 800, height = 600)
  hist(feature_data, breaks = num_bins, main = paste(feature_name, "Distribution"), xlab = paste(feature_name, feature_units))
  dev.off()
  
  # Logarithmic transformation of the feature values
  log_hist_filename <- paste0(feature_column_name, "_log_histogram.png")
  png(file.path(path_to_save_results_species, log_hist_filename), width = 800, height = 600)
  log_feature <- log(feature_data + 1)  # Adding 1 to avoid log(0)
  num_bins_log <- min(30, max(10, length(na.omit(log_feature)) / 10))
  hist(log_feature, main = paste("Log-Transformed", feature_name, "Distribution"), xlab = paste0("Log(", feature_name, ")"), col = "lightblue", breaks = num_bins_log)
  dev.off()
  
  # # 4. Box Plot
  # # Linear scale
  # boxplot(feature_data, main = paste(feature_name, "Quartiles"), ylab = paste(feature_name, feature_units), col = "lightblue")
  # # Logarithmic scale
  # boxplot(log_feature, main = paste("Logarithmic", feature_name, "Quartiles"), ylab = paste0("Log(", feature_name, ")"), col = "lightblue")
  
  # # 5. Density Plot
  # # Linear scale
  # plot(density(feature_data, na.rm = TRUE), main = paste(feature_name, "Density Plot"), xlab = paste(feature_name, feature_units), col = "blue")
  # # Logarithmic scale
  # plot(density(log_feature, na.rm = TRUE), main = paste0("Log(", feature_name, ") Density Plot"), xlab = paste0("Log(", feature_name, ")"), col = "blue")
  
  # Facet Plot- outliers versus non-outliers
  library(ggplot2)
  
  # Linear scale
  p <- ggplot(species_data_clean, aes(x = !!sym(feature_column_name))) +
    geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7) +
    facet_wrap(~ Outliers) +
    labs(title = paste(feature_name, "Distribution by Outlier Status"), 
         x = paste(feature_name, feature_units), 
         y = "Frequency") +
    theme_minimal(base_size = 15) + 
    theme(
      strip.text = element_text(size = 12, face = "bold"),  # Make facet titles bold and size 12
      strip.background = element_rect(fill = "white", color = "black"),  # White background for facet labels
      plot.background = element_rect(fill = "white"),  # Set the overall background to white
      panel.background = element_rect(fill = "white"),  # Set panel background to white
      panel.grid.major = element_line(color = "gray80"),  # Adjust grid line color
      panel.grid.minor = element_line(color = "gray90")  # Adjust minor grid lines
    )
  
  facet_plot_filename <- paste0(feature_column_name, "_facet_plot_outliers.png")
  ggsave(file.path(path_to_save_results_species, facet_plot_filename), plot = p, width = 8, height = 6, dpi = 300)
  
  
  # TODO Add this option to the function
  # # Features with integer values, such as NBS
  # ggplot(location_data_labeled, aes(x = !!sym(column_name))) +
  #   geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  #   facet_wrap(~ Outliers) +
  #   labs(title = paste(feature_name, "Distribution by Outlier Status"), 
  #        x = paste(feature_name, feature_units), 
  #        y = "Frequency") +
  #   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Adjusts breaks to show only integer values
  #   theme_minimal()
  
  # Logarithmic scale
  # Add log_feature as a new column to species_data_clean
  species_data_clean$log_feature <- log_feature
  p = ggplot(species_data_clean, aes(x = log_feature)) +
      geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7) +
      facet_wrap(~ Outliers) +
      labs(title = paste(feature_name, "Log-Transformed Distribution by Outlier Status"), 
           x = paste("Log of", feature_name, feature_units), 
            y = "Frequency") +
      theme_minimal(base_size = 15) +
      theme(
        strip.text = element_text(size = 12, face = "bold"),  # Make facet titles bold and size 12
        strip.background = element_rect(fill = "white", color = "black"),  # White background for facet labels
        plot.background = element_rect(fill = "white"),  # Set the overall background to white
        panel.background = element_rect(fill = "white"),  # Set panel background to white
        panel.grid.major = element_line(color = "gray80"),  # Adjust grid line color
        panel.grid.minor = element_line(color = "gray90")  # Adjust minor grid lines
      )
  facet_log_plot_filename <- paste0(feature_column_name, "_facet_log_plot_outliers.png")
  ggsave(file.path(path_to_save_results_species, facet_log_plot_filename), plot = p, width = 8, height = 6, dpi = 300)
  
  
  
}