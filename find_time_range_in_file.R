library(DBI)
library(RSQLite)

# Load the CSV file
file_name <- "eminem_night18.csv"
# file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Outliers/"
# file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Raw_tracks/"
file_path <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/tagging_database/Raw_data/"
csv_file_path <- paste0(file_path, file_name) 
df <- read.csv(csv_file_path)

# # Convert the Datetime column to Date or POSIXct if needed
# df$Datetime <- as.POSIXct(df$Datetime, format = "%Y-%m-%d %H:%M:%S") # Adjust format as necessary

# Find the minimum and maximum dates
min_date <- min(df$TIME, na.rm = TRUE)
max_date <- max(df$TIME, na.rm = TRUE)

# Convert to human readable dates
source(paste0(path_to_atlas_data_analysis_repo,"time_conversions.R"))
min_date <- unix_timestamp_to_human_date(min_date)
max_date <- unix_timestamp_to_human_date(max_date)

# Print the results
cat("Minimum Date:", min_date, "\n")
cat("Maximum Date:", max_date, "\n")

# Save as sqlite
sqlite_file_name <- sub("\\.csv$", ".sqlite", file_name)
sqlite_file_path <- paste0(file_path, sqlite_file_name)
conn <- dbConnect(RSQLite::SQLite(), dbname = sqlite_file_path)
dbWriteTable(conn, "LOCALIZATIONS", df, overwrite=TRUE)
dbDisconnect(conn)





