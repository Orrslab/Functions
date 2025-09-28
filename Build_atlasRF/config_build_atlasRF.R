
config <- list(
  
  # Workflow Steps to Run
  workflow_steps_to_run = list (
    establish_labeled_db = FALSE,
    calculate_features = FALSE,
    clean_features_data = FALSE,
    split_data_to_training_and_test = FALSE,
    tune_hyper_parameters = FALSE,
    train_atlasRF = TRUE,
    run_atlasRF_on_test_set = TRUE
  ),
  
  # Paths
  paths = list(
    path_to_db = "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Code_orchestration_test/Build_atlasRF/Labeled_DB_test",
    # path_to_db = "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Labeled_data_DB/Visual_Filter_DB",
    base_stations_info_path = "C:/Users/netat/Documents/Movement_Ecology/ATLAS/Base_stations_beacons_info/Base_stations_info.csv",
    folder_of_beacons_info_tables = "C:/Users/netat/Documents/Movement_Ecology/ATLAS/Base_stations_beacons_info",
    filename_beacons_detection_ratio_table = "beacons_detection_ratio_per_hour.Rds",
    filename_base_stations_summary_per_beacon = "base_stations_summary_per_beacon.Rds",
    folder_of_feature_results = "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Code_orchestration_test/Build_atlasRF/Data_with_features",
    filename_cleaned_feature_data_all_species = "Features_data_cleaned_for_RF_all_species.sqlite",
    atlasRF_results_folder = "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Code_orchestration_test/Build_atlasRF/RF_results",
    training_set_filename = "training_set.rds",
    test_set_filename = "test_set.rds",
    filename_atlasRF_model_tuned = "atlasRF_model_tuned.rds",
    filename_atlasRF_model_trained = "atlasRF_model_final_trained_on_full_training_set.rds"
  ),
  
  # Feature Calculation Settings
  feature_settings = list(
    half_time_window_size_sec = 25,
    low_beacon_detection_fraction = 0.6 
  ),
  
  # Training and Test Settings
  training_and_test_settings = list(
    # Random Forest will be built for the following species
    desired_species_id = "GJ",
    # Which fraction of the data you want to use for training of the random forest model?
    training_data_fraction = 0.8,
    # Define the columns that are not considered as features and should be excluded from the training set
    non_feature_column_names = c("TAG", "X", "Y", "Z", "lat", "lon",
                                  "TIME", "dateTime", "DAY", "geometry", 
                                 "Species_id", "X_mean", "Y_mean", "X_median", "Y_median")
  ),
  
  # atlasRF Hyperparameter Settings
  atlasRF_hyperparameter_settings = list(
    number_of_cross_validation_folds = 5,
    # number of features to consider at each split
    mtry_values = c(5, 10, 15)
  ),
  
  # atlasRF Training Settings
  atlasRF_training_settings = list(
    # Set the parameter values for the model training
    mtry = 10,
    splitrule = "gini",
    min_node_size = 1,
    # Assign weights- Give higher weight to outliers (1 means no extra weight)
    outlier_weight = 1
  ),
  
  # atlasRF Test Settings
  atlasRF_test_settings = list(
    tree_vote_optional_thresholds = seq(0.1, 0.9, by = 0.05)
  )
  
)

### Define paths that depend on base paths ###

# Labeled Database Establishment
config$paths$labeling_info_path <- file.path(config$paths$path_to_db, "Labeling_metadata.xlsx")
config$paths$combined_species_data_folder <- file.path(config$paths$path_to_db, "Combined_species_data")
config$paths$tags_metadata_file_path <- file.path(config$paths$combined_species_data_folder, "metadata_per_tag.csv")
config$paths$species_metadata_file_path <- file.path(config$paths$combined_species_data_folder, "metadata_per_species.csv")

