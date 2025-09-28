# Define feature information
features_info <- list(
  list(
    feature_column_name = "Speed_m_s", 
    feature_name = "Speed", 
    feature_units = "m/s"
  ),
  list(
    feature_column_name = "STD", 
    feature_name = "Standard Deviation of the distance", 
    feature_units = "m"
  ),
  list(
    feature_column_name = "cos_turning_angle", 
    feature_name = "cos(turning_angle)", 
    feature_units = ""
  )
)

# Save the list as an RData file
save(features_info, file = file.path(getwd(), "Build_atlasRF", "Feature_engineering", "features_info.RData"))