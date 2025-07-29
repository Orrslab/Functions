library(dplyr)
library(sf)
library(RColorBrewer)
library(leaflet)
library(htmltools)

source(paste0(getwd(), "/time_conversions.R"))

#' Create an interactive map for a single ATLAS dataset
#'
#' @param dd A data frame that contains X (longitude) and Y (latitude) columns 
#'   in Israeli Transverse Mercator (EPSG:2039).
#' @param color_column A column name to determine the point color (e.g., "Outliers").
#' @param MapProvider Map tile provider for background. Default is 'Esri.WorldImagery'.
#' @param legend_title Title for the legend (optional).
#'
#' @return An interactive leaflet map.
#'
interactive_map_single_atlas_dataset_color_by_vector <- function(dd,
                                                 color_column = "Outliers",
                                                 MapProvider = 'Esri.WorldImagery',
                                                 legend_title = NULL) {
  
  # List of optional variables to initialize with NA if missing
  varlist <- c("PENALTY", "spd", "angl", "stdVarXY")
  for (varname in varlist) {
    if (!(varname %in% names(dd)))
      dd[[varname]] <- NA
  }
  
  # Validate required coordinate columns
  if (!all(c("X", "Y") %in% colnames(dd))) {
    stop("Data must contain 'X' and 'Y' columns (in EPSG:2039)")
  }
  
  if (nrow(dd) == 0) {
    stop("You must provide at least one data point")
  }
  
  # Define CRS
  itm <- 2039   # EPSG code for Israeli Transverse Mercator
  wgs84 <- 4326 # EPSG code for WGS84
  
  # Convert to sf and transform to WGS84
  dd_sf <- st_as_sf(dd, coords = c("X", "Y"), crs = itm)
  dd_sf <- st_transform(dd_sf, crs = wgs84)
  
  # Group points by TAG and create LINESTRINGs
  dd_lines <- dd_sf %>%
    group_by(TAG) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  # Add formatted datetime
  dd_sf$dateTimeFormatted <- unix_timestamp_to_human_date(dd_sf$TIME)
  
  # Create a color palette based on the unique values in color_column
  color_vals <- as.factor(dd_sf[[color_column]])
  color_palette <- colorFactor(palette = brewer.pal(min(length(levels(color_vals)), 8), "Dark2"),
                               domain = levels(color_vals))
  
  # Generate the interactive map
  map <- leaflet() %>%
    addProviderTiles(MapProvider, options = providerTileOptions(opacity = 0.8)) %>%
    addPolylines(data = dd_lines, weight = 1, opacity = 1, color = "black") %>%
    addCircleMarkers(data = dd_sf,
                     color = ~color_palette(get(color_column)),
                     fillOpacity = 1,
                     stroke = FALSE,
                     radius = 5,
                     popup = ~htmlEscape(paste0("Date+Time = ", dateTimeFormatted,
                                                ", TIME = ", TIME,
                                                ", Tag = ", sprintf("%04d", TAG %% 10000),
                                                ", ", color_column, " = ", get(color_column)))) %>%
    addLegend("bottomright",
              pal = color_palette,
              values = dd_sf[[color_column]],
              title = legend_title %||% color_column,
              opacity = 1) %>%
    addScaleBar(position = "bottomleft",
                options = scaleBarOptions(imperial = FALSE, maxWidth = 200))
  
  return(map)
}