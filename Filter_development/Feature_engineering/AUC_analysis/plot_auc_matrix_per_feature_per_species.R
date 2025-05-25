library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

plot_auc_matrix_per_feature_per_species <- function(auc_per_feature_per_species) {
  # Ensure it's a data.table
  dt <- as.data.table(auc_per_feature_per_species)
  
  # Compute average AUC per feature (ignoring NA)
  feature_means <- dt[, .(mean_auc = mean(AUC, na.rm = TRUE)), by = Feature]
  
  # Order features by mean AUC (descending for top to bottom)
  dt <- merge(dt, feature_means, by = "Feature")
  dt <- dt[order(mean_auc)]  # ascending order
  dt$Feature <- factor(dt$Feature, levels = unique(dt$Feature))  # lock order
  
  # Plot
  p <- ggplot(dt, aes(x = Species_id, y = Feature, fill = AUC)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(AUC, 2)), size = 3) +
    scale_fill_viridis(name = "AUC", option = "C", na.value = "grey90", limits = c(0, 1)) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      axis.title = element_blank()
    ) +
    labs(title = "AUC per Feature per Species")
  
  return(p)
}