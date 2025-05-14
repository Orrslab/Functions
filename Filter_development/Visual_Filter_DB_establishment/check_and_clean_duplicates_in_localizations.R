
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