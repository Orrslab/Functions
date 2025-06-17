# Analyze the missed base stations, which were closer than the participating ones

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

source(file.path(getwd(), "Filter_development/Feature_engineering/load_data_with_features.R"))

# USER'S INPUT BEGIN

missed_bs_analysis_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Missed_base_stations_analysis"

# tables_to_load <- c("LOCALIZATIONS", 
#                     "DETECTIONS", 
#                     "PARTICIPATING_BASE_STATIONS", 
#                     "MISSED_BASE_STATIONS", 
#                     "PROPERTIES")

tables_to_load <- c("LOCALIZATIONS", 
                    "MISSED_BASE_STATIONS")

# USER'S INPUT END

# Load the features data
features_data <- load_data_with_features(tables_to_load)

localizations_data <- features_data$LOCALIZATIONS
missed_bs <- features_data$MISSED_BASE_STATIONS

#### Outliers proportion in the locations with missed base stations

# Filter only the rows where num_missed_bs > 0
loc_with_missed_bs <- localizations_data[localizations_data$num_missed_bs > 0, ]

# Count how many of them are Outliers == "valid" (valid points)
valid_points <- sum(loc_with_missed_bs$Outliers == "valid")

# Count how many of them are Outliers == "outliers"
outlier_points <- sum(loc_with_missed_bs$Outliers == "outlier")

# Print the results
cat("Number of valid points with missed base stations (Outliers == 0):", valid_points, "\n")
cat("Number of outlier points with missed base stations (Outliers == 1):", outlier_points, "\n")

#### Distribution of the number of missed base stations

## All locations

# Count occurrences of each num_missed_bs value
missed_counts <- table(loc_with_missed_bs$num_missed_bs)

# Save pie plot as a PNG file
png(file.path(missed_bs_analysis_folder, "num_missed_bs_bar_plot.png"), width = 800, height = 600)

# Create the plot
barplot(missed_counts,
        main = "Number of Missed Base Stations- all locations",
        col = "olivedrab",  # olive green
        xlab = "Number of Missed Base Stations",
        ylab = "Frequency",
        las = 2)

# Close the graphics device
dev.off()

## Outliers

# Filter data: only outliers with num_missed_bs > 0
outlier_data <- localizations_data[localizations_data$Outliers == "outlier" & localizations_data$num_missed_bs > 0, ]

# Create a frequency table of num_missed_bs
outlier_counts <- table(outlier_data$num_missed_bs)

# Create and save the bar plot
png(file.path(missed_bs_analysis_folder, "num_missed_bs_barplot_outliers.png"), width = 1000, height = 600)

barplot(outlier_counts,
        main = "Number of Missed Base Stations among Outliers",
        col = "firebrick4",
        xlab = "Number of Missed Base Stations",
        ylab = "Frequency",
        las = 2)

dev.off()

## Valid Points

# Filter data: only outliers with num_missed_bs > 0
valid_data <- localizations_data[localizations_data$Outliers == "valid" & localizations_data$num_missed_bs > 0, ]

# Create a frequency table of num_missed_bs
valid_counts <- table(valid_data$num_missed_bs)

# Create and save the bar plot
png(file.path(missed_bs_analysis_folder, "num_missed_bs_barplot_valid_points.png"), width = 1000, height = 600)

barplot(valid_counts,
        main = "Number of Missed Base Stations among Outliers",
        col = "antiquewhite",
        xlab = "Number of Missed Base Stations",
        ylab = "Frequency",
        las = 2)

dev.off()

#### Distribution of the ID numbers of the missed base stations

## All Locations

# Extract the last two digits of each missed_bs_id (as character)
missed_bs$missed_bs_short <- substr(as.character(missed_bs$missed_bs_id), 
                                    nchar(as.character(missed_bs$missed_bs_id)) - 1, 
                                    nchar(as.character(missed_bs$missed_bs_id)))

# Create a frequency table of the shortened IDs
missed_counts <- table(missed_bs$missed_bs_short)

# Save pie plot
png(file.path(missed_bs_analysis_folder, "missed_bs_id_all_locations.png"), width = 800, height = 600)

barplot(missed_counts,
        main = "Distribution of the ID of the Missed Base Stations- All Locations",
        col = "olivedrab",
        xlab = "ID number of the Missed Base Station",
        ylab = "Frequency",
        las = 2)

dev.off()

## Outliers

# Make sure TIME columns are the same type
missed_bs$TIME <- as.POSIXct(missed_bs$TIME)
localizations_data$TIME <- as.POSIXct(localizations_data$TIME)

# Merge data to get Outliers info
missed_bs_merged <- merge(missed_bs, localizations_data[, c("TAG", "TIME", "Outliers")],
                          by = c("TAG", "TIME"), all.x = TRUE)

# Keep only outliers
missed_bs_outliers <- missed_bs_merged[missed_bs_merged$Outliers == "outlier", ]

# Extract last two digits
missed_bs_outliers$missed_bs_short <- substr(as.character(missed_bs_outliers$missed_bs_id),
                                             nchar(as.character(missed_bs_outliers$missed_bs_id)) - 1,
                                             nchar(as.character(missed_bs_outliers$missed_bs_id)))

# Frequency table
missed_outlier_counts <- table(missed_bs_outliers$missed_bs_short)

# Save bar plot
png(file.path(missed_bs_analysis_folder, "missed_bs_id_barplot_outliers.png"), width = 1000, height = 600)

barplot(missed_outlier_counts,
        main = "Distribution of the ID of the Missed Base Stations- Outliers",
        col = "firebrick4",
        xlab = "ID number of the Missed Base Station",
        ylab = "Frequency",
        las = 2)

dev.off()

## Valid Points

# Keep only valid points
missed_bs_valid <- missed_bs_merged[missed_bs_merged$Outliers == "valid", ]

# Extract last two digits
missed_bs_valid$missed_bs_short <- substr(as.character(missed_bs_valid$missed_bs_id),
                                             nchar(as.character(missed_bs_valid$missed_bs_id)) - 1,
                                             nchar(as.character(missed_bs_valid$missed_bs_id)))

# Frequency table
missed_valid_counts <- table(missed_bs_valid$missed_bs_short)

# Save bar plot
png(file.path(missed_bs_analysis_folder, "missed_bs_id_barplot_valid.png"), width = 1000, height = 600)

barplot(missed_valid_counts,
        main = "Distribution of the ID of the Missed Base Stations- Valid Points",
        col = "antiquewhite",
        xlab = "ID number of the Missed Base Station",
        ylab = "Frequency",
        las = 2)

dev.off()
