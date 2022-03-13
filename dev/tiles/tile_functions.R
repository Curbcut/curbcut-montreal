#### MTS FUNCTIONS #############################################################


# Upload tile source ------------------------------------------------------

upload_tile_source <- function(df, id, username, access_token) {
  
  # Initialize tempfile
  tmp <- tempfile(fileext = ".json")
  
  # Write JSON to tempfile
  df |> 
    geojsonsf::sf_geojson() |> 
    paste0(collapse = " ") |> 
    featurecollection() |> 
    ndgeo_write(tmp)
  
  # Construct system call
  out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                username, '/', id, '?access_token=', access_token, 
                '" -F file=@', tmp, 
                ' --header "Content-Type: multipart/form-data"')
  
  system(out)
  
}


# Create tileset ----------------------------------------------------------

create_tileset <- function(tileset, recipe, username, access_token) {
  
  POST(
    url = paste0("https://api.mapbox.com/tilesets/v1/",
                 username, ".", tileset),
    query = list(access_token = access_token),
    body = recipe,
    content_type("application/json")
  )
  
}


# Update tileset ----------------------------------------------------------

# TKTK DOESN'T WORK
update_tileset <- function(tileset, recipe, username, access_token) {
  
  PATCH(
    url = paste0("https://api.mapbox.com/tilesets/v1/",
                 username, ".", tileset, "/recipe"),
    query = list(access_token = access_token),
    body = recipe,
    content_type("application/json")
  )
  
}



# Publish tileset ---------------------------------------------------------

publish_tileset <- function(tileset, username, access_token) {
  POST(
    url = paste0("https://api.mapbox.com/tilesets/v1/", username, ".", tileset,
                 "/publish"),
    query = list(access_token = access_token)
  )
}
  


# Create style ------------------------------------------------------------

create_style <- function(style, username, access_token) {
  
  POST(
    url = paste0("https://api.mapbox.com/styles/v1/", username),
    query = list(access_token = access_token),
    body = style,
    content_type("application/json")
  )
  
}

  