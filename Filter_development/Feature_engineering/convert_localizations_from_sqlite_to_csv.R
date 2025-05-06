source(file.path(getwd(), "convert_sqlite_table_to_csv.R"))

# From where to take the sqlite file
sqlite_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
filename <- "GJ_features_eng"

# Where to save the csv file
output_csv_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features/csv_files"

# Define input and output file paths
sqlite_path <- file.path(sqlite_folder, paste0(filename, ".sqlite"))
output_csv_path <- file.path(output_csv_folder, paste0(filename, ".csv"))

convert_sqlite_table_to_csv(sqlite_path = sqlite_path,
                            table_name = "LOCALIZATIONS",
                            output_csv_path = output_csv_path)
