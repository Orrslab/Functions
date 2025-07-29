
apply_stops_rule_to_data <- function(localization_data, stop_filter_rules) {
  
  # Start with a logical vector of TRUEs
  is_stop <- rep(TRUE, nrow(localization_data))
  
  # Apply each rule as a vectorized operation
  for (rule in stop_filter_rules) {
    col <- rule$column
    type <- rule$type
    value <- rule$value
    
    # Apply the rule using vectorized comparison
    condition <- switch(
      type,
      ">" = localization_data[[col]] > value,
      ">=" = localization_data[[col]] >= value,
      "<" = localization_data[[col]] < value,
      "<=" = localization_data[[col]] <= value,
      "==" = localization_data[[col]] == value,
      "!=" = localization_data[[col]] != value,
      TRUE  # fallback in case of unknown operator
    )
    
    # Handle NAs: treat NA as FALSE
    condition[is.na(condition)] <- FALSE
    
    # Combine with previous conditions using AND logic
    is_stop <- is_stop & condition
  }
  
  # Assign result to Is_stop column
  localization_data$Is_stop <- as.integer(is_stop)
  
  return(localization_data)
  
}