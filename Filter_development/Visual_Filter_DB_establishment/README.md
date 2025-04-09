# Outliers Annotation Database Instructions- How to Establish the Database

## 1. Prepare the Annotated Data
- You will receive SQLite files containing annotated data from researchers.

## 2. Set Up Your Database Folder Structure
- Create a main folder where your database will be stored.
- Inside this folder:
  - Create a subfolder for each species using their `species_id` as the folder name (e.g., `BO`for Barn Owl etc.).
  - Copy the corresponding SQLite files for each species into their respective subfolders.

## 3. Create the Species Metadata File
- In the main database folder, create an Excel file named `Species_metadata.xlsx`.
- This file should include two columns:
  - **Species_ID**: A unique identifier for each species.
  - **Species_name**: The full name of the species.
- Fill in the species ID and corresponding species name for each dataset you received.

## 4. Run `get_data_range_and_check_multiplications.R` Script for each species
This script:
- Generates a metadata file named `{species_id}_files_metadata.csv` for each species, and saves it inside the species folder.
- Creates a bar plot showing the time ranges covered by each data file, grouped by tag number.
- Checks for multiple annotations of the same data point.
- Merges all the data from the SQLite files and saves it as a single SQLite file for each species.
- Generates a `/Combined_species_data/metadata_per_tag.csv` file containing the following columns:
  - `Species_ID`, `TAG`, `Start_time`, `End_time`, `Num_records`, `Data_source`, `Reviewer`, `Filter_applied` 

### Before Running the Script:
- Create a new folder named **`Combined_species_data`** inside your main database folder.
- Provide the following input parameters in the script:
  - `species_id <- "BO"` → Insert the species ID you're processing.
  - `reviewer_name <- "Neta Tsur"` → Enter the name of the data reviewer.
  - `data_source <- "ATLAS system Harod"` → Enter the data source (or another relevant source).
  - `filter_applied <- "Visual Filter"` → Specify the filter or tool used for the outliers annotation.
  - `path_to_db` → Provide the path to the folder where the database is stored.
  - Adjust the time axis resolution of the bar plot based on the amount of data:
    - For small datasets, set the resolution to show ticks every hour.
    - For long-term datasets (e.g., spanning months or years), use a monthly or yearly resolution.

### Script Outputs:
- A metadata file `{species_id}_files_metadata.csv` including:
  - `Species_ID`, `Start_time`, `End_time`, `Num_records`, `Reviewer`, `Filter_applied`
- A bar plot visualizing the time range of each data file (saved in the species folder).
- Console message output:
  - **No duplicate annotations found:** A success message will appear.
  - **Duplicates detected:** A file named `duplicates.csv` will be saved with details about conflicting rows.
    - This prevents errors caused by conflicting annotations (i.e., when the same location point is marked as both valid and an outlier).

---

## 5. Run `get_metadata_per_species.R` Script
This script:
- Combines metadata across all species from the file `/Combined_species_data/metadata_per_tag.csv`.
- Generates a `/Combined_species_data/metadata_per_species.csv` file containing the following columns:
  - `Species_ID`, `Num_tags`, `Start_time`, `End_time`, `Num_records`
  
## 6. Data Visualization
To create interactive maps of the data in the sqlite files from the Visual Filter App, run /Data_visualization/create_maps_of_all_sqlite_files_in_DB.R
It is recommended to run this script for each species separately.