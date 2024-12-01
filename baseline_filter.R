
# Function to identify outliers based on given thresholds- for the manual tagging
baseline_filter <- function(df, speed_threshold, std_threshold, nbs_threshold) {
  
  # Ensure the 'Outliers' column exists
  if (!"Outliers" %in% colnames(df)) {
    df$Outliers <- 0  # Initialize Outliers column to 0 if it doesn't exist
  }
  
  # Apply threshold conditions for speed, STD, and number of base stations
  df$Outliers <- ifelse(
    df$Speed_m_s > speed_threshold | df$STD > std_threshold | df$NBS < nbs_threshold, 
    1,  # Set Outliers to 1 if any condition is met
    df$Outliers  # Otherwise, keep the current value (either 0 or previously set)
  )
  
  return(df)
}