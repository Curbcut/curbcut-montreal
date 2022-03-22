#### MAP OBSERVER FUNCTION #####################################################

observe_map <- function(map_input) {
  
  # Initialize objects
  out <- NULL
  zoom <- map_input$zoom
  lat <- map_input$center[[2]]
  lon <- map_input$center[[1]]
  
  # Exit early if the map isn't sufficiently zoomed in
  if (zoom < 13) return(out)
  
  # Get POIs; currently just Montreal Stories
  poi <- st_set_geometry(stories, "geometry")
  poi <- poi[c("name", "geometry")]
    
  # Find distance from map centre to the POIs
  dist <-
    c(lon, lat) |> 
    st_point() |> 
    st_sfc(crs = 4326) |> 
    st_distance(poi)
  
  # If any POI is within 2000 m of centre, return it
  out <- poi[as.vector(units::drop_units(dist)) < 2000,]$name
  if (length(out) == 0) out <- NULL
  
  return(out)
}