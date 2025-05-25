#' Create a map of two different ATLAS datasets
#' 
#' @description
#' plot an interactive map of the ATLAS location points, a popup of information about each point, 
#' and trajectories between consequent points- from a single data set, for example a single track
#' 
#' @param dd1,dd2 The data sets that should be plotted.
#' @param MapProvider The map tiles provider (default is 'Esri.WorldImagery').
#' @param legendLabels Labels of 'dd1' and 'dd2' for the map's legend. Example: c("1", "2")
#' 
#' @return An interactive map that displays two different tracks (datasets) with distinct colors and popups.
#' 
#' @import leaflet
#' @import sf
#' @importFrom RColorBrewer brewer.pal  
#'
interactive_map_two_atlas_datasets <- function(dd1,dd2,
                                               color_dd1 = "red", color_dd2 = "blue",
                                               MapProvider='Esri.WorldImagery', 
                                               legendLabels=c("1", "2")) 
{
  
  library(leaflet)
  library(sf)
  library(RColorBrewer)
  library(dplyr)
  library(htmltools)
  
  # Remove rows with NA in X or Y columns
  dd1 <- dd1[!is.na(dd1$X) & !is.na(dd1$Y), ]
  dd2 <- dd2[!is.na(dd2$X) & !is.na(dd2$Y), ]
  
  # Define CRS for the datasets
  itm <- 2039  # EPSG code for your local CRS
  wgs84 <- 4326  # EPSG code for WGS84
  
  # Convert data frames to sf objects
  dd1_sf <- st_as_sf(dd1, coords = c("X", "Y"), crs = itm)
  dd2_sf <- st_as_sf(dd2, coords = c("X", "Y"), crs = itm)
  
  # Transform to WGS84
  llpd1_sf <- st_transform(dd1_sf, crs = wgs84)
  llpd2_sf <- st_transform(dd2_sf, crs = wgs84)
  
  # Group points by TAG, and then create LINESTRING for polylines
  # Grouping by tag is done to avoid connecting the geometries of different animals
  llpd1_lines <- llpd1_sf %>%
    group_by(TAG) %>%
    summarize(do_union = FALSE) %>%  # Prevent union of geometries
    st_cast("LINESTRING")
  
  llpd2_lines <- llpd2_sf %>%
    group_by(TAG) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  # Convert Unix timestamp to a UTC humandate in ATLAS format
  source(paste0(getwd(), "/time_conversions.R"))
  llpd1_sf$dateTimeFormatted <- unix_timestamp_to_human_date(llpd1_sf$TIME)
  llpd2_sf$dateTimeFormatted <- unix_timestamp_to_human_date(llpd2_sf$TIME)
  
  # Define color palette
  col <- brewer.pal(n = 6, name = 'Dark2')
  
  ll <- leaflet() %>%
    
    # Add the base map
    addProviderTiles(MapProvider, options = providerTileOptions(opacity = 0.8)) %>%
    
    # # Add lines that connect the point locations included in 'dd1'
    # addPolylines(data = llpd1_lines, weight = 1, opacity = 1, color = "#E66100", group = legendLabels[1]) %>%
    
    # Add circles at the locations of the first dataset 'dd1'
    addCircles(data = llpd1_sf, weight = 3, fillOpacity = 1, color = color_dd1, group = legendLabels[1],
               popup = ~htmlEscape(paste0("1:time=", as.character(llpd1_sf$dateTimeFormatted),
                                          ", TIME=", as.character(llpd1_sf$TIME),
                                          ", NBS=", as.character(llpd1_sf$NBS),
                                          ", Speed_m_s=", as.character(round(llpd1_sf$Speed_m_s)),
                                          ", STD=", as.character(round(llpd1_sf$STD)),
                                          ", TAG=", llpd1_sf$TAG))) %>%
    
    # Add lines that connect the point locations included in 'dd2'
    addPolylines(data = llpd2_lines, weight = 1, opacity = 1, color = color_dd2, group = legendLabels[2]) %>%
    
    # Add circles at the locations of the first dataset 'dd2'
    addCircles(data = llpd2_sf, weight = 3, fillOpacity = 1, color = color_dd2, group = legendLabels[2],
               popup = ~htmlEscape(paste0("2:time=", as.character(llpd2_sf$dateTimeFormatted),
                                          ", TIME=", as.character(llpd2_sf$TIME),
                                          ", NBS=", as.character(llpd2_sf$NBS),
                                          ", Speed_m_s=", as.character(round(llpd2_sf$Speed_m_s)),
                                          ", STD=", as.character(round(llpd2_sf$STD)),
                                          ", TAG=", llpd2_sf$TAG))) %>%
    
    # Add a scale bar to the map
    addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial = FALSE, maxWidth = 200)) %>%
    
    # Add a layer control to the map to allow users to toggle the visibility of the different tracks.
    addLayersControl(
      overlayGroups = legendLabels,
      options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  
  return(ll)
  # print(ll) # To display the map without returning it.
}

# # Usage example
# map <- interactive_map_two_atlas_datasets(dd1 = outliers_dataframe,
#                                           dd2 = valid_points_dataframe,
#                                           legendLabels = c("Outliers",
#                                                            "Valid Points"))
# print(map)
