library(ggplot2)
library(tidyr)
library(dplyr)
library(caret)

# USER'S INPUT BEGIN
random_forest_results_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/Species_models/WB"
rf_model_filename <- "rf_model_final_trained_on_full_training_set.rds"

# Set threshold for the labels prediction
threshold <- 0.6

# USER'S INPUT END

# # Load trained model and test set
# rf_model_final <- readRDS(file.path(random_forest_results_folder, rf_model_filename))
# test_data <- readRDS(file.path(random_forest_results_folder, "test_set.rds"))

##### Generate confusion table for each species

# Get list of unique species
unique_species <- unique(test_data$Species_id)

# Create folder to save confusion matrices per species
dir.create(file.path(random_forest_results_folder, "confusion_matrices_by_species"), showWarnings = FALSE)

# Loop over each species
for (sp in unique_species) {
  cat("Processing species:", sp, "\n")
  
  # Filter test data for the current species
  sp_data <- test_data[test_data$Species_id == sp, ]
  sp_features <- sp_data[, !(names(sp_data) %in% non_feature_column_names)]
  
  # Predict probabilities
  sp_pred_probs <- predict(rf_model_final, data = sp_features, type = "response")$predictions
  sp_pred_classes <- ifelse(sp_pred_probs[, "outlier"] > threshold, "outlier", "valid")
  sp_pred_classes <- factor(sp_pred_classes, levels = c("outlier", "valid"))
  
  # Confusion matrix
  sp_conf_mat <- confusionMatrix(sp_pred_classes, sp_data$Outliers, positive = "outlier")
  
  # Save to CSV
  sp_conf_table <- as.data.frame(sp_conf_mat$table)
  write.csv(sp_conf_table, file = file.path(random_forest_results_folder, "confusion_matrices_by_species", paste0("confusion_matrix_", sp, ".csv")), row.names = FALSE)
}

##########
# Performance metrices per species
# יצירת רשימת מדדים עבור כל מין
species_metrics_list <- list()

for (sp in unique_species) {
  cat("Calculating metrics for species:", sp, "\n")
  
  # סינון לדאטה של המין הנוכחי
  sp_data <- test_data[test_data$Species_id == sp, ]
  sp_features <- sp_data[, !(names(sp_data) %in% non_feature_column_names)]
  
  # ניבוי עבור המין הזה
  sp_pred_probs <- predict(rf_model_final, data = sp_features, type = "response")$predictions
  sp_pred_classes <- ifelse(sp_pred_probs[, "outlier"] > threshold, "outlier", "valid")
  sp_pred_classes <- factor(sp_pred_classes, levels = c("outlier", "valid"))
  
  # מטריצת בלבול
  sp_conf_mat <- confusionMatrix(sp_pred_classes, sp_data$Outliers, positive = "outlier")
  
  # שליפת מדדים (Overall ו-byClass)
  sp_overall <- as.data.frame(t(sp_conf_mat$overall))
  sp_byClass <- as.data.frame(t(sp_conf_mat$byClass))
  
  # הוספת עמודת Species_id כדי שנוכל לאחד אח"כ
  sp_metrics <- cbind(Species_id = sp, sp_byClass)
  species_metrics_list[[sp]] <- sp_metrics
}

# איחוד כל המדדים לטבלה אחת
species_metrics_df <- do.call(rbind, species_metrics_list)

# המרת הנתונים לפורמט "ארוך" לצורך גרף ggplot
library(tidyr)
long_df <- pivot_longer(species_metrics_df,
                        cols = c("Sensitivity", "Specificity"),
                        names_to = "Metric",
                        values_to = "Value")

# ציור הגרף
p <- ggplot(long_df, aes(x = Species_id, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sensitivity and Specificity per Species",
       x = "Species ID",
       y = "Value") +
  theme_minimal() +
  scale_fill_manual(values = c("Sensitivity" = "#1b9e77", "Specificity" = "#d95f02")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

# Save the bar plot
ggsave(file.path(random_forest_results_folder, "sensitivity_specificity_per_species.png"), plot = p, width = 8, height = 5, dpi = 300, bg = "white")

# Outliers percentage per species
species_outlier_rates <- test_data %>%
  group_by(Species_id) %>%
  summarise(outlier_rate = mean(Outliers == "outlier"))

write.csv(species_outlier_rates, file.path(random_forest_results_folder, "species_outlier_rates.csv"), row.names = FALSE)

# חיבור בין אחוזי שגיאות למדדי המודל
joined_df <- species_metrics_df %>%
  left_join(species_outlier_rates, by = "Species_id")

# ציור: קשר בין אחוז שגיאות ל-Sensitivity
p <- ggplot(joined_df, aes(x = outlier_rate, y = Sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sensitivity vs Outlier Rate",
       x = "Outlier Rate per Species",
       y = "Sensitivity") +
  theme_minimal()

print(p)

# Save the bar plot
ggsave(file.path(random_forest_results_folder, "sensitivity_vs_outlier_rate.png"), plot = p, width = 8, height = 5, dpi = 300, bg = "white")

# שמירה לקובץ CSV
write.csv(species_metrics_df,
          file = file.path(random_forest_results_folder, "performance_metrics_by_species.csv"),
          row.names = FALSE)
##########