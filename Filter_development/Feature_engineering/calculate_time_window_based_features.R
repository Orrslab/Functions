library(data.table)

source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_features_in_time_window.R"))

calculate_time_window_based_features <- function(localizations_data, half_window_size_sec = 20) {
  
  # Convert to data.table
  loc_dt <- as.data.table(localizations_data)
  
  # Ensure data is sorted (important for speed)
  setkey(loc_dt, TAG, TIME)
  
  # Convert the time window size from seconds to milliseconds
  half_window_millisec <- half_window_size_sec * 1000
  
  # Pre-allocate list for new columns (each element will be a named list)
  features_list <- vector("list", nrow(loc_dt))
  
  # Optional: Progress bar
  pb <- txtProgressBar(min = 0, max = nrow(loc_dt), style = 3)
  
  
  for (i in seq_len(nrow(loc_dt))) {
    tag_i <- loc_dt[i, TAG]
    time_i <- loc_dt[i, TIME]  # This is in milliseconds
    
    # Subset the data within the window for the same TAG
    window_data <- loc_dt[
      TAG == tag_i & TIME >= (time_i - half_window_millisec) & TIME <= (time_i + half_window_millisec)
    ]
    
    # Compute features for the current window
    features_list[[i]] <- calculate_features_in_time_window(window_data)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Combine the features into a data.table
  features_dt <- rbindlist(features_list, fill = TRUE)
  
  # Bind features directly onto original data
  loc_dt <- cbind(loc_dt, features_dt)
  
  return(as.data.frame(loc_dt))
}