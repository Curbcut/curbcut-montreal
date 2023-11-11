# # https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/services/Defavorisation/defavorisation_2006_2011_2016_2021/MapServer
# 
# # EXAMPLE
# base_url <- "https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/services/Defavorisation/defavorisation_2006_2011_2016_2021/MapServer/2/query"
# 
# query_params <- list(
#   where = "1=1", # to get all the data; no filter
#   outFields = "*", # to get all fields
#   outSR = "4326", # output spatial reference; EPSG:4326 is WGS84 lat/long
#   f = "geojson", # output format
#   returnGeometry = "true" # to ensure geometry is included
# )
# 
# # Make the GET request
# response <- httr::GET(url = base_url, query = query_params)
# 
# # Content parsing straight to a spatial dataframe using sf
# sf_data <- sf::st_read(content(response, "text"), quiet = TRUE)
# 
# sf_data
# sf_data |> .mv()
