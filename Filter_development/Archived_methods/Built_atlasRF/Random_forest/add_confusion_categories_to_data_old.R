#' Add Confusion Table Categories to a Data Frame
#'
#' This function adds a new column called `confusion_category` to a data frame
#' containing observed and predicted outlier labels. The new column classifies
#' each row as one of: "True Positive", "True Negative", "False Positive", or "False Negative",
#' based on a comparison of the `Outliers` and `predicted_outlier` columns.
#'
#' @param data_with_label_predictions A data frame that must contain the columns:
#'   \describe{
#'     \item{Outliers}{Observed labels, expected to be either "outlier" or "valid"}
#'     \item{predicted_outlier}{Predicted labels, expected to be either "outlier" or "valid"}
#'   }
#' @return A modified data frame with an additional factor column `confusion_category` indicating the
#'   confusion matrix classification for each row.
#' @examples
#' df <- data.frame(
#'   Outliers = c("outlier", "valid", "valid", "outlier"),
#'   predicted_outlier = c("outlier", "valid", "outlier", "valid")
#' )
#' add_confusion_categories_to_data(df)
#'
#' @export
add_confusion_categories_to_data <- function(data_with_label_predictions) {
  
  # Check that required columns exist
  required_columns <- c("Outliers", "predicted_outlier")
  missing_columns <- setdiff(required_columns, names(data_with_label_predictions))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Force both columns to be factors with the right levels
  valid_levels <- c("outlier", "valid")
  
  data_with_label_predictions$Outliers <- factor(data_with_label_predictions$Outliers,
                                                 levels = valid_levels)
  data_with_label_predictions$predicted_outlier <- factor(data_with_label_predictions$predicted_outlier,
                                                          levels = valid_levels)
  
  # Create an empty column for the confution table categories
  data_with_label_predictions$confusion_category <- NA  
  
  # Get the values of the different confusion table categories
  data_with_label_predictions$confusion_category[data_with_label_predictions$Outliers == "outlier" & data_with_label_predictions$predicted_outlier == "outlier"] <- "True Positive"
  data_with_label_predictions$confusion_category[data_with_label_predictions$Outliers == "valid"   & data_with_label_predictions$predicted_outlier == "valid"]   <- "True Negative"
  data_with_label_predictions$confusion_category[data_with_label_predictions$Outliers == "valid"   & data_with_label_predictions$predicted_outlier == "outlier"] <- "False Positive"
  data_with_label_predictions$confusion_category[data_with_label_predictions$Outliers == "outlier" & data_with_label_predictions$predicted_outlier == "valid"]   <- "False Negative"
  
  # Convert the categories' column to factor with a certain order of the categories
  data_with_label_predictions$confusion_category <- factor(data_with_label_predictions$confusion_category,
                                         levels = c("True Positive", "True Negative", "False Positive", "False Negative"))
  
  return(data_with_label_predictions)
}




