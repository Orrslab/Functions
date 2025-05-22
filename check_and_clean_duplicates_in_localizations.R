#' Check and optionally clean duplicates in the localizations table.
#'
#' Identifies duplicate records in a localization dataset based on combinations of `TAG` and `TIME`.
#' Optionally removes duplicates while prioritizing rows marked as `Outliers == 1`, then `Outliers == 2`,
#' in case the Outliers values in the duplicates are different.
#'
#' @param localization_data A data frame containing at least the columns `TAG`, `TIME`, and `Outliers`.
#' @param clean_duplicates Logical; if `TRUE`, the function will remove duplicates based on `TAG` and `TIME`, keeping rows with `Outliers == 1` or `Outliers == 2` first. Defaults to `FALSE`.
#'
#' @return A data frame with duplicate entries reported (and optionally removed).
#' Prints the number and content of duplicate `(TAG, TIME)` combinations before and/or after cleaning.
#'
#' @details Duplicate records are determined based on identical values of `TAG` and `TIME`.
#' Note that due to floating-point precision or minor time differences (e.g., within a few seconds), some near-duplicates may not be detected.
#'
#' @examples
#' # Detect duplicates without cleaning
#' cleaned_data <- check_and_clean_duplicates_in_localizations(localizations_data, clean_duplicates = FALSE)
#'
#' # Detect and clean duplicates
#' cleaned_data <- check_and_clean_duplicates_in_localizations(localizations_data, clean_duplicates = TRUE)
#'
#' @export
check_and_clean_duplicates_in_localizations <- function(localization_data, clean_duplicates = FALSE) {
  
  # Check for duplicates based on both TAG and TIME- returns a few wrong duplicates whose TIME values are different in just a few seconds
  duplicates <- localization_data %>%
    group_by(TAG, TIME) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # # Print message depending on whether duplicates were found
  # if (nrow(duplicates) > 0) {
  #   cat("Number of duplicated (TAG, TIME) combinations:", nrow(duplicates), "\n")
  #   print(duplicates)
  # } else {
  #   cat("There are no duplicates in the data.\n")
  # }
  
  if (clean_duplicates) {
    # Clean the duplicates, while prioritizing leaving the Outliers = 1
    localization_data <- localization_data %>%
      arrange(TAG, TIME, desc(Outliers == 1), desc(Outliers == 2)) %>%
      distinct(TAG, TIME, .keep_all = TRUE)
  }
  
  # Check for duplicates based on both TAG and TIME- returns a few wrong duplicates whose TIME values are different in just a few seconds
  duplicates <- localization_data %>%
    group_by(TAG, TIME) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Print message depending on whether duplicates were found
  if (nrow(duplicates) > 0) {
    cat("Number of duplicated (TAG, TIME) combinations:", nrow(duplicates), "\n")
    print(duplicates)
  } else {
    cat("There are no duplicates in the data.\n")
  }
  
  return(localization_data)
  
}
