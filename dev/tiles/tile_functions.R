#### MTS FUNCTIONS #############################################################


# List tile sources -------------------------------------------------------

list_tile_sources <- function(username = "sus-mcgill", 
                              access_token = .sus_token) {
  
  httr::GET(paste0("https://api.mapbox.com/tilesets/v1/sources/", username),
            query = list(access_token = access_token)) |> 
    httr::content()
  
}

# Upload tile source ------------------------------------------------------

upload_tile_source <- function(df, id, username = "sus-mcgill", 
                               access_token = .sus_token) {
  
  # Initialize tempfile
  tmp <- tempfile(fileext = ".json")
  
  # Write JSON to tempfile
  df |> 
    geojsonsf::sf_geojson() |> 
    paste0(collapse = " ") |> 
    geojson::featurecollection() |> 
    geojson::ndgeo_write(tmp)
  
  # Construct system call
  out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                username, '/', id, '?access_token=', access_token, 
                '" -F file=@', tmp, 
                ' --header "Content-Type: multipart/form-data"')
  
  system(out)
  
}


# Delete tileset source ---------------------------------------------------

delete_tileset_source <- function(id, username = "sus-mcgill", 
                                  access_token = .sus_token) {
  
  httr::DELETE(paste0("https://api.mapbox.com/tilesets/v1/sources/", username,
                      "/", id), query = list(access_token = access_token)
  )
}


# Create tileset ----------------------------------------------------------

create_tileset <- function(tileset, recipe, username = "sus-mcgill", 
                           access_token = .sus_token) {
  
  httr::POST(
    url = paste0("https://api.mapbox.com/tilesets/v1/",
                 username, ".", tileset),
    query = list(access_token = access_token),
    body = recipe,
    httr::content_type("application/json")
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

publish_tileset <- function(tileset, username = "sus-mcgill", 
                            access_token = .sus_token) {
  httr::POST(
    url = paste0("https://api.mapbox.com/tilesets/v1/", username, ".", tileset,
                 "/publish"),
    query = list(access_token = access_token)
  )
}
  

# Manipulate variables ----------------------------------------------------

trans_table <-
  tibble(group = c("1 - 1", "2 - 1", "3 - 1", "1 - 2", "2 - 2", "3 - 2",
                   "1 - 3", "2 - 3", "3 - 3"),
         vals = 6:14)

trans_var_internal <- function(x) {
  args <- tibble::deframe(trans_table)
  args <- c(list(EXPR = x), args, list(0))
  do.call(switch, args)
}

trans_var <- function(x) {
  as.integer(sapply(x, trans_var_internal, USE.NAMES = FALSE))
}

  