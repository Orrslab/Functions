library(tidyverse)
library(caret)

source(file.path(getwd(), "ATLAS_data_retrieval/config_atlasRF.R"))

folder_path <- folder_to_save_labeled_data

# Get list of all *_confusion_matrix.csv files in the folder
confusion_files <- list.files(path = folder_path, pattern = "_confusion_matrix\\.csv$", full.names = TRUE)

# Read and combine all confusion tables
combined_confusion <- confusion_files %>%
  map_dfr(read_csv, col_types = cols()) %>%
  group_by(Prediction, Reference) %>%
  summarise(Freq = sum(Freq), .groups = "drop")

# Save the combined confusion table
write_csv(combined_confusion, file.path(folder_path, "confusion_matrix_all_tags.csv"))

# Expand the table to a full confusion matrix
conf_mat_table <- combined_confusion %>%
  pivot_wider(names_from = Reference, values_from = Freq, values_fill = 0) %>%
  column_to_rownames("Prediction")

# Make sure rows and columns are in the correct order
all_levels <- c("valid", "outlier")
conf_mat_table <- conf_mat_table[all_levels, all_levels]

# Generate vectors of predictions and references to use with caret::confusionMatrix
pred_vector <- rep(rownames(conf_mat_table), rowSums(conf_mat_table))
ref_vector <- rep(rep(colnames(conf_mat_table), each = nrow(conf_mat_table)), unlist(as.vector(conf_mat_table)))

# Calculate metrics
cm <- confusionMatrix(factor(pred_vector, levels = all_levels),
                      factor(ref_vector, levels = all_levels),
                      positive = "valid")

# Extract metrics
metrics <- tibble(
  Accuracy = cm$overall["Accuracy"],
  Kappa = cm$overall["Kappa"],
  Sensitivity = cm$byClass["Sensitivity"],
  Specificity = cm$byClass["Specificity"],
  Precision = cm$byClass["Precision"],
  F1 = cm$byClass["F1"]
)

# Save metrics
write_csv(metrics, file.path(folder_path, "metrics_all_tags.csv"))