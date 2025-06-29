library(data.table)
library(sp)
library(geosphere)
library(dplyr)

#' Calculate Base Stations Distribution Features
#'
#' For each localization, this function:
#' - Computes the area of the convex hull polygon formed by the participating base stations.
#' - Evaluates whether the location is inside the polygon.
#'
#' @param matched A data.table containing matched localization and participating base station data.
#'   Must contain columns: TAG, TIME, loc_lat, loc_lon, bs_lat, bs_lon.
#'
#' @param localizations_data A data.table of localization points to which the features will be added.
#'   Must contain TAG and TIME.
#'
#' @return `localizations_data` with two new columns:
#'   - `bs_polygon_area`: area of the polygon (in square meters)
#'   - `is_loc_inside_bs_polygon`: logical, TRUE if location is within the polygon, FALSE otherwise
#'
#' @export
calculate_base_stations_distribution_features <- function(matched, localizations_data) {
  # Ensure input is data.table
  matched <- as.data.table(matched)
  localizations_data <- as.data.table(localizations_data)
  
  # Add unique ID to group by location
  matched[, loc_id := .GRP, by = .(TAG, TIME)]
  
  # For each location
  results <- matched[, {
    # Get coordinates of base stations
    bs_coords <- cbind(bs_lon, bs_lat)
    
    # Require at least 3 points to form a polygon
    if (nrow(bs_coords) < 3) {
      list(bs_polygon_area = NA_real_, is_loc_inside_bs_polygon = NA)
    } else {
      # Get convex hull of the base stations
      hull_indices <- chull(bs_coords)
      hull_coords <- bs_coords[c(hull_indices, hull_indices[1]), ]  # close polygon
      
      # Compute area in square meters
      area <- geosphere::areaPolygon(hull_coords)
      
      # Check if location is inside the polygon
      loc_point <- c(loc_lon[1], loc_lat[1])
      inside <- point.in.polygon(loc_point[1], loc_point[2], hull_coords[,1], hull_coords[,2]) > 0
      
      list(bs_polygon_area = area, is_loc_inside_bs_polygon = inside)
    }
  }, by = loc_id]
  
  ########### DEBUGGING #####################
  # # דוגמה מתוך matched
  # example_loc <- matched[TAG == matched$TAG[10000] & TIME == matched$TIME[10000]]
  # 
  # print("תחנות בסיס:")
  # print(example_loc[, .(bs_lon, bs_lat)])
  # 
  # # מיקום החיה
  # animal_point <- c(example_loc$loc_lon[1], example_loc$loc_lat[1])
  # print(paste("מיקום החיה: lon =", animal_point[1], ", lat =", animal_point[2]))
  # 
  # # 1. צור את הפוליגון
  # bs_coords <- example_loc[, .(bs_lon, bs_lat)]
  # coords_matrix <- as.matrix(bs_coords)
  # 
  # # סדר לפי הקונבקס הול
  # hull_indices <- chull(coords_matrix)
  # hull_coords <- coords_matrix[c(hull_indices, hull_indices[1]), ]  # סגירת הפוליגון
  # print("קואורדינטות הפוליגון:")
  # print(hull_coords)
  # 
  # # 2. חשב שטח
  # polygon_area <- geosphere::areaPolygon(hull_coords)  # in square meters
  # print(paste("שטח הפוליגון: ", polygon_area, "מ״ר"))
  # 
  # # 3. האם מיקום החיה בתוך הפוליגון?
  # is_inside <- sp::point.in.polygon(
  #   point.x = animal_point[1],
  #   point.y = animal_point[2],
  #   pol.x = hull_coords[, 1],
  #   pol.y = hull_coords[, 2]
  # ) > 0
  # print(paste("האם המיקום בתוך הפוליגון?", is_inside))
  # 
  # # הגדר שם קובץ לשמירה
  # png_filename <- "bs_polygon_plot_Barn_Owl_10000.png"
  # 
  # # פתח קובץ PNG
  # png(filename = png_filename, width = 800, height = 800)
  # 
  # # ציור הפוליגון ותחנות הבסיס
  # plot(bs_coords$bs_lon, bs_coords$bs_lat, type = "n", asp = 1)
  # polygon(hull_coords[,1], hull_coords[,2], border = "blue", col = "#ccccff")
  # points(bs_coords$bs_lon, bs_coords$bs_lat, pch = 16, col = "blue")
  # points(example_loc$loc_lon, example_loc$loc_lat, pch = 19, col = "red")  # מיקום החיה
  # 
  # # סגור את הקובץ
  # dev.off()
  
  ################
  
  # Convert matched to miliseconds to match the time units in localizations_data.
  matched[, TIME := TIME * 1000]
  
  # Merge results back to localizations_data using loc_id
  loc_ids <- unique(matched[, .(TAG, TIME, loc_id)])
  localizations_data <- merge(localizations_data, loc_ids, by = c("TAG", "TIME"), all.x = TRUE)
  localizations_data <- merge(localizations_data, results, by = "loc_id", all.x = TRUE)
  
  # Clean up
  localizations_data[, loc_id := NULL]
  
  return(localizations_data)
}