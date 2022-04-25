#### MAP OBSERVER FUNCTION #####################################################

observe_map <- function(map_input) {
  
  # Initialize objects
  out <- NULL
  zoom <- map_input$zoom
  lat <- map_input$latitude
  lon <- map_input$longitude
  
  # Exit early if the map isn't sufficiently zoomed in
  if (zoom < 13) return(out)
  
  # Get POIs; currently just Montreal Stories
  poi <- stories[c("name", "lon", "lat")]
    
  # Find distance from map centre to the POIs
  dist <- get_dist(poi[c("lon", "lat")], c(lon, lat))

  # If any POI is within 2000 m of centre, return it
  out <- poi$name[dist < 2000]
  if (length(out) == 0) out <- NULL
  
  return(out)
}