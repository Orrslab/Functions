# Add the coordinates of the nesting box to each location point

library(dplyr)

add_nesting_box_coordinates <- function(localization_data, nesting_bo_metadata) {
  
  # Convert TIME to Date (from milliseconds to POSIXct then Date)
  localization_data <- localization_data %>%
    mutate(obs_date = as.Date(as.POSIXct(TIME / 1000, origin = "1970-01-01", tz = "UTC")))
  
  # Convert Since and Until to Date
  nesting_bo_metadata <- nesting_bo_metadata %>%
    mutate(
      Since = as.Date(Since, format = "%d/%m/%Y"),
      Until = as.Date(Until, format = "%d/%m/%Y")
    )
  
  # Split metadata into single-box and multi-box per TAG
  meta_counts <- nesting_bo_metadata %>%
    count(Tag_number, name = "box_count")
  
  single_box <- nesting_bo_metadata %>%
    inner_join(meta_counts %>% filter(box_count == 1), by = "Tag_number")
  
  multi_box <- nesting_bo_metadata %>%
    inner_join(meta_counts %>% filter(box_count > 1), by = "Tag_number")
  
  # --- Join for TAGs with a single nesting box ---
  localization_single <- localization_data %>%
    inner_join(single_box, by = c("TAG" = "Tag_number")) %>%
    mutate(
      nesting_box_lat = Latitude,
      nesting_box_lon = Longitude
    ) %>%
    dplyr::select(-Latitude, -Longitude, -Since, -Until, -obs_date)
  
  # --- Join for TAGs with multiple nesting boxes: match by date ---
  localization_multi <- localization_data %>%
    semi_join(multi_box, by = c("TAG" = "Tag_number")) %>%
    left_join(multi_box, by = c("TAG" = "Tag_number")) %>%
    filter(obs_date >= Since & obs_date <= Until) %>%
    mutate(
      nesting_box_lat = Latitude,
      nesting_box_lon = Longitude
    ) %>%
    dplyr::select(-Latitude, -Longitude, -Since, -Until, -obs_date)
  
  # Combine the two parts
  final_localization <- bind_rows(localization_single, localization_multi)
  
  return(final_localization)
  
}