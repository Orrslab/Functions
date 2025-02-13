# Helper function to initialize the base map
initialize_atl_mapleaf <- function(MapProvider = map_provider, tile_opacity = 0.8) {
  leaflet() %>%
    addProviderTiles(MapProvider, options = providerTileOptions(opacity = tile_opacity)) %>%
    addDrawToolbar(
      targetGroup = "drawn_polygon",
      polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fillOpacity = 0.2)),
      circleOptions = FALSE, # Disable circle drawing
      rectangleOptions = FALSE, # Disable rectangle drawing
      markerOptions = FALSE, # Disable marker drawing
      circleMarkerOptions = FALSE, # Disable circle marker drawing
      polylineOptions = FALSE, # Disable polyline drawing
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
    ) %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE, maxWidth = 200))
}