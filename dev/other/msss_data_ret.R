msss_ret <- function(url) {
  base_url <- url
  
  # prepare the query with parameters to fetch all features
  # as geoJSON, which is a good intermediate format for conversion to a shapefile
  query_params <- list(
    where = "1=1", # to get all the data; no filter
    outFields = "*", # to get all fields
    outSR = "4326", # output spatial reference; EPSG:4326 is WGS84 lat/long
    f = "geojson", # output format
    returnGeometry = "true" # to ensure geometry is included
  )
  
  # Make the GET request
  response <- httr::GET(url = base_url, query = query_params)
  
  # Content parsing straight to a spatial dataframe using sf
  sf_data <- sf::st_read(httr::content(response, "text"), quiet = TRUE)
}