
library(ranger)

#' Train an atlasRF Model on the Full Training Dataset
#'
#' This function trains an ATLAS Random Forest model using the `ranger` package on a pre-specified
#' training dataset. The model is trained to classify outliers, with optional weighting for
#' outlier cases, and is saved to a file for downstream use.
#'
#' @param config A list containing configuration settings for paths and training parameters:
#'   - \code{config$paths$random_forest_results_folder}: Folder path to save/load model and datasets.
#'   - \code{config$paths$training_set_filename}: Name of the training dataset file (RDS format).
#'   - \code{config$random_forest_training_settings$mtry}: Number of variables to possibly split at each node.
#'   - \code{config$random_forest_training_settings$splitrule}: Splitting rule for the Random Forest (e.g., "gini").
#'   - \code{config$random_forest_training_settings$min_node_size}: Minimal node size for tree growth.
#'   - \code{config$random_forest_training_settings$outlier_weight}: Optional weight to assign to outlier cases.
#'
#' @details
#' The function performs the following steps:
#' 1. Loads the training dataset from the specified file.
#' 2. Ensures that the `Outliers` column is a factor, which is required for classification.
#' 3. Optionally assigns higher weights to outlier cases to handle class imbalance.
#' 4. Trains a Random Forest classifier using the `ranger` package with specified hyperparameters:
#'    - `mtry`: number of features considered at each split,
#'    - `splitrule`: splitting criterion,
#'    - `min.node.size`: minimum number of samples in a terminal node.
#' 5. Sets `probability = TRUE` to obtain predicted probabilities instead of class labels.
#' 6. Computes variable importance using the impurity-based measure.
#' 7. Saves the trained model as an RDS file for later use.
#' 8. Prints a summary of the trained model.
#'
#' @return
#' The function does not return an R object but saves the trained Random Forest model to the
#' path: \code{random_forest_results_folder/rf_model_final_trained_on_full_training_set.rds}.
#'
#' @examples
#' \dontrun{
#' config <- list(
#'   paths = list(
#'     random_forest_results_folder = "results/",
#'     training_set_filename = "train_data.rds"
#'   ),
#'   random_forest_training_settings = list(
#'     mtry = 10,
#'     splitrule = "gini",
#'     min_node_size = 1,
#'     outlier_weight = 5
#'   )
#' )
#' main_train_random_forest_model(config)
#' }
#'
#' @import ranger
#' @export
main_train_atlasRF <- function(config) {
  
  message("### STARTED THE MAIN SCRIPT OF RANDOM FOREST TRAINING. ###")
  
  # Load the training dataset
  train_data <- readRDS(file.path(config$paths$random_forest_results_folder, config$paths$training_set_filename))
  
  # Make sure the label column is a factor type (required for running the random forest)
  train_data$Outliers <- as.factor(train_data$Outliers)
  
  # # Load caret model from tuning stage
  # caret_model <- readRDS(file.path(random_forest_results_folder, "rf_model.rds"))
  # 
  # # Extract the best hyperparameters from the validated model
  # mtry_value <- caret_model$bestTune$mtry
  # splitrule_value <- caret_model$bestTune$splitrule
  # min_node_size_value <- caret_model$bestTune$min.node.size
  
  # Create a vector of weights for the outliers
  weights <- ifelse(train_data$Outliers == "1", config$random_forest_training_settings$outlier_weight, 1)
  
  # Train the final model on the full training dataset
  rf_model_final <- ranger(
    formula = Outliers ~ .,
    data = train_data,
    mtry = config$random_forest_training_settings$mtry,
    splitrule = config$random_forest_training_settings$splitrule,
    min.node.size = config$random_forest_training_settings$min_node_size,
    probability = TRUE,
    importance = "impurity",
    seed = 42,
    case.weights = weights,
    num.threads = 1
  )
  
  # Save the trained model to file
  saveRDS(rf_model_final, file.path(config$paths$random_forest_results_folder, config$paths$filename_random_forest_model_trained))
  
  # Print model summary
  print(rf_model_final)
  
  message("atlasRF IS READY FOR USE!")
  
}

