#' Add Confusion Table Categories to a Data Frame
#'
#' This function adds a new column called `confusion_category` to a data frame
#' containing observed and predicted outlier labels. The new column classifies
#' each row as one of: "True Positive", "True Negative", "False Positive", or "False Negative",
#' based on a comparison of specified reference and predicted label columns.
#'
#' @param data_with_label_predictions A data frame that contains the reference and predicted labels.
#' @param reference_label_col A string specifying the column name of the observed labels (e.g., "Outliers").
#' @param predicted_label_col A string specifying the column name of the predicted labels (e.g., "predicted_outlier").
#'
#' @return A modified data frame with an additional factor column `confusion_category` indicating the
#'   confusion matrix classification for each row.
#'
#' @examples
#' df <- data.frame(
#'   true = c("outlier", "valid", "valid", "outlier"),
#'   pred = c("outlier", "valid", "outlier", "valid")
#' )
#' add_confusion_categories_to_data(df, reference_label_col = "true", predicted_label_col = "pred")
#'
#' @export
add_confusion_categories_to_data <- function(data_with_label_predictions,
                                             reference_label_col = "Outliers",
                                             predicted_label_col = "predicted_outlier") {
  
  # Check that required columns exist
  required_columns <- c(reference_label_col, predicted_label_col)
  missing_columns <- setdiff(required_columns, names(data_with_label_predictions))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Force both columns to be factors with the right levels
  valid_levels <- c("outlier", "valid")
  
  data_with_label_predictions[[reference_label_col]] <- factor(
    data_with_label_predictions[[reference_label_col]], levels = valid_levels
  )
  data_with_label_predictions[[predicted_label_col]] <- factor(
    data_with_label_predictions[[predicted_label_col]], levels = valid_levels
  )
  
  # Initialize column
  data_with_label_predictions$confusion_category <- NA
  
  # Compute categories
  data_with_label_predictions$confusion_category[
    data_with_label_predictions[[reference_label_col]] == "outlier" &
      data_with_label_predictions[[predicted_label_col]] == "outlier"] <- "True Positive"
  
  data_with_label_predictions$confusion_category[
    data_with_label_predictions[[reference_label_col]] == "valid" &
      data_with_label_predictions[[predicted_label_col]] == "valid"] <- "True Negative"
  
  data_with_label_predictions$confusion_category[
    data_with_label_predictions[[reference_label_col]] == "valid" &
      data_with_label_predictions[[predicted_label_col]] == "outlier"] <- "False Positive"
  
  data_with_label_predictions$confusion_category[
    data_with_label_predictions[[reference_label_col]] == "outlier" &
      data_with_label_predictions[[predicted_label_col]] == "valid"] <- "False Negative"
  
  # Convert to factor
  data_with_label_predictions$confusion_category <- factor(
    data_with_label_predictions$confusion_category,
    levels = c("True Positive", "True Negative", "False Positive", "False Negative")
  )
  
  return(data_with_label_predictions)
}