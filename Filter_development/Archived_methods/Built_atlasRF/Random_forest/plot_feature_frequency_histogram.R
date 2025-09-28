# 
# library(readxl)
# library(dplyr)
# library(purrr)
# 
# ### USER'S INPUT BEGIN ###
# input_table_path <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/Species_models/Feature_importance_all_species.xlsx"
# output_results_path <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/Species_models"
# ### USER'S INPUT END ###
# 
# # Path to your Excel file - update this accordingly
# file_path <- input_table_path
# sheet_name <- "Sheet1"  # Update if your sheet name is different
# 
# # Read the entire table
# data <- read_excel(file_path, sheet = sheet_name)
# 
# # The first row contains species names (column headers)
# species_names <- names(data)[-1]  # Exclude the first column (feature names)
# 
# # The rest of the rows contain the features for each species
# features_matrix <- as.data.frame(data[, -1])  # Remove first column (feature names)
# rownames(features_matrix) <- data[[1]]  # Set row names as feature names from the first column
# 
# # Convert the matrix into a long-format tibble of species and their features
# features_long <- tibble(
#   Species = rep(species_names, each = nrow(features_matrix)),
#   Feature = unlist(features_matrix, use.names = FALSE)
# )
# 
# # Get the unique sorted list of features for each species
# species_features <- features_long %>%
#   group_by(Species) %>%
#   summarise(UniqueFeatures = list(sort(unique(Feature))))
# 
# # Check if all species have exactly the same set of features
# all_same <- all(map_lgl(species_features$UniqueFeatures, ~ identical(.x, species_features$UniqueFeatures[[1]])))
# 
# if (all_same) {
#   message("✅ All species have exactly the same 20 features.")
# } else {
#   message("⚠️ There are differences in features between species.")
#   
#   # Print differences
#   print(species_features)
# }


library(readxl)
library(tidyverse)

### USER'S INPUT BEGIN ###
input_table_path <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/Species_models/Feature_importance_all_species.xlsx"
output_results_path <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Random_Forest_Model/Species_models"
num_of_first_features <- 55  # Change this to any number between 1 and 20
### USER'S INPUT END ###

# Read the feature importance data from the Excel file
feature_data <- read_excel(input_table_path)

# Calculate max allowed features (number of rows - 1)
max_allowed_features <- nrow(feature_data)

# Validate num_of_first_features value
if(num_of_first_features < 1 || num_of_first_features > max_allowed_features) {
  stop(paste0("Error: num_of_first_features must be between 1 and ", max_allowed_features))
}

# Select only the first 'num_of_first_features' rows (top features per species)
top_features_data <- feature_data[1:num_of_first_features, ]

# Pivot to long format: each row = (Species, Feature)
features_long <- top_features_data %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Feature") %>%
  filter(!is.na(Feature) & Feature != "")

# Count frequency of each feature across all species (only top features)
feature_counts <- features_long %>%
  count(Feature, name = "Frequency") %>%
  arrange(desc(Frequency))

# Create filenames including the number of features
csv_filename <- paste0("feature_frequency_top_", num_of_first_features, "_features.csv")
png_filename <- paste0("feature_frequency_top_", num_of_first_features, "_features.png")

# Save frequency table to CSV
write.csv(feature_counts, file.path(output_results_path, csv_filename), row.names = FALSE)

# Plot histogram
feature_plot <- ggplot(feature_counts, aes(x = reorder(Feature, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = paste("Frequency of Top", num_of_first_features, "Features Across All Species"),
    x = "Feature",
    y = "Number of Occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot as PNG
ggsave(file.path(output_results_path, png_filename), plot = feature_plot, width = 12, height = 6, dpi = 300, bg = "white")