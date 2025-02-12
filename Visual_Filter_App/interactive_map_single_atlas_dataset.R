library(dplyr)
library(sf)
library(RColorBrewer)
library(leaflet)
library(htmltools)

source(paste0(getwd(), "/time_conversions.R"))


#' Create a map of a single ATLAS dataset
#'
#' @description
#' plot an interactive map of the ATLAS location points, a popup of information about each point, 
#' and trajectories between consequent points- from a single data set, for example a single track
#' 
#' @param dd A data frame that contains X (longitude) and Y (latitude) columns, 
#' representing spatial coordinates of the location in itm coordinates: Israeli Transverse Mercator (EPSG:2039)
#' @param MapProvider A string defining the map tile provider for the background map. 
#' The default is 'Esri.WorldImagery'. 
#' An alternative option ('OpenStreetMap.BZH') is commented out but can be used by changing the parameter.
#' 
#' @return An interactive leaflet map with the plotted data points, info popups, and paths
#' 
#' @import leaflet
#' @import sp
#' @importFrom RColorBrewer brewer.pal
#'
interactive_map_single_atlas_dataset <- function(dd,MapProvider='Esri.WorldImagery', legendLabels=c("1")) # 'OpenStreetMap.BZH'
{
  
  # A list of variables to check or add to the data frame if they don't exist
  varlist =c("PENALTY","spd","angl","stdVarXY")
  
  # For each variable in 'varlist', check if it exists in the data frame 'dd'. 
  # If not, create a column with NA values.
  for (varname in varlist) {
    if (!(varname %in% names(dd)))
      dd[,varname] <- NA
  } 
  
  # Check if the required columns "X" and "Y" (coordinates) are present in 'dd'.
  # If either is missing, stop the function and return an error.
  if(! all(c("X","Y") %in% colnames(dd))) {
    Er <- simpleError("data must contain X and Y columns")
    stop(Er)
  }
  
  # If the data frame is empty (no rows), stop the function and return an error.
  if( nrow(dd)==0) {
    Er <- simpleError("you must provide at least a single data point")
    stop(Er)
  }
  
  # Define CRS for the datasets
  itm <- 2039  # EPSG code for your local CRS
  wgs84 <- 4326  # EPSG code for WGS84
  
  # Convert data frames to sf objects
  dd_sf <- st_as_sf(dd, coords = c("X", "Y"), crs = itm)
  
  # Transform to WGS84
  llpd_sf <- st_transform(dd_sf, crs = wgs84)
  
  # Group points by TAG, and then create LINESTRING for polylines
  # Grouping by tag is done to avoid connecting the geometries of different animals
  llpd_lines <- llpd_sf %>%
    group_by(TAG) %>%
    summarize(do_union = FALSE) %>%  # Prevent union of geometries
    st_cast("LINESTRING")
  
  # Convert Unix timestamp to a UTC humandate in ATLAS format
  llpd_sf$dateTimeFormatted <- unix_timestamp_to_human_date(llpd_sf$TIME)
  
  # Define color palette
  col <- brewer.pal(n = 6, name = 'Dark2')
  
  ll <- leaflet() %>%
    
    # Add the base map
    addProviderTiles(MapProvider, options = providerTileOptions(opacity = 0.8)) %>%
    
    # Add lines that connect the point locations included in 'dd'
    addPolylines(data = llpd_lines, weight = 1, opacity = 1, color = "#5D3A9B") %>%
    
    # Add circles at the locations of the first dataset 'dd1'
    addCircles(data = llpd_sf, weight = 1, fillOpacity = 1, color = "#5D3A9B", group = legendLabels[1],
               popup = ~htmlEscape(paste0("Date+Time=", as.character(llpd_sf$dateTimeFormatted),
                                          ", TIME=", as.character(llpd_sf$TIME),
                                          ", Tag Number=", sprintf("%04d", llpd_sf$TAG %% 10000)))) %>%
    
    # Add a scale bar to the map
    addScaleBar(position = c("bottomleft"), 
                options = scaleBarOptions(imperial = FALSE, maxWidth = 200)) %>%
    
    # Add a layer control to the map to allow users to toggle the visibility of the different tracks.
    addLayersControl(
      overlayGroups = legendLabels,
      options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
  
  return(ll)
  # print(ll) # To display the map without returning it.
}

