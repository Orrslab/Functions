
library(ggplot2)
library(data.table)

plot_cohens_d_heatmap_per_species <- function(cohens_d_df) {
  # Convert to data.table
  cohens_d_df <- as.data.table(cohens_d_df)
  
  # Compute mean d per feature (signed), then order features accordingly
  feature_order <- cohens_d_df[, .(mean_d = mean(Cohens_d, na.rm = TRUE)), by = Feature][order(mean_d)]$Feature
  
  cohens_d_df[, Feature := factor(Feature, levels = feature_order)]
  
  p <- ggplot(cohens_d_df, aes(x = Species_id, y = Feature, fill = Cohens_d)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Cohens_d, 2)), size = 3) +
    scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkorange", midpoint = 0,
                         name = "Cohen's d") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank()) +
    labs(title = "Cohen's d per Feature and Species",
         x = "Species", y = "Feature")
  
  return(p)
}