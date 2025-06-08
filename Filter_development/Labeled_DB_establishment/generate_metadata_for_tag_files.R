
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/get_metadata_from_all_sqlite_files_in_folder.R"))

generate_metadata_for_tag_files <- function(path_to_species,
                                            species_id,
                                            reviewer_name,
                                            data_source,
                                            filter_applied) {
  
  # Retreive the metadata from all the sqlite files of the labeled data sent from the reviewers ###
  files_metadata <- get_metadata_from_all_sqlite_files_in_folder(path_to_species)
  
  # Add the Species ID, Reviewer's name and data source
  files_metadata <- files_metadata %>%
    mutate(
      Species_ID = species_id,    # Assuming species_id is a single value for all rows
      Reviewer = reviewer_name,         # Assuming Reviewer is a single value for all rows
      Data_source = data_source,
      Filter_applied = filter_applied
    )
  
  # Re-order the column names
  files_metadata <- files_metadata %>%
    dplyr::select(Species_ID, TAG, Start_time, End_time, Num_records, Data_source, Reviewer, Filter_applied)
  
  return(files_metadata)
}
