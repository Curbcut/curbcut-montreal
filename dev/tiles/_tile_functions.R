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


# List tilesets -----------------------------------------------------------

list_tilesets <- function(username = "sus-mcgill", access_token = .sus_token) {
  
  httr::GET(paste0("https://api.mapbox.com/tilesets/v1/", username),
            query = list(access_token = access_token)) |> 
    httr::content()
  
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


# Delete tileset ----------------------------------------------------------

delete_tileset <- function(id, username = "sus-mcgill", 
                           access_token = .sus_token) {
  
  httr::DELETE(paste0("https://api.mapbox.com/tilesets/v1/", username,
                      ".", id), query = list(access_token = access_token)
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
         vals = as.character(6:14))

trans_var_internal <- function(x) {
  args <- tibble::deframe(trans_table)
  args <- c(list(EXPR = x), args, list(0))
  do.call(switch, args)
}

trans_var <- function(x) {
  sapply(x, trans_var_internal, USE.NAMES = FALSE)
}


# Create recipes ----------------------------------------------------------

create_recipe <- function(layer_names, source, minzoom, maxzoom,
                          layer_size = NULL, simplification_zoom = NULL, 
                          bbox = NULL, recipe_name) {
  out <- list()
  out$recipe$version <- 1
  out$name <- recipe_name
  layers <- list()
  
  out$recipe$layers <- 
    if (length(layer_names) > 1) {
      map(layer_names, function(layer) {
        layers[[layer]]$source <- source[[layer]]
        layers[[layer]]$minzoom <- minzoom[[layer]]
        layers[[layer]]$maxzoom <- maxzoom[[layer]]
        if (!is.null(layer_size[[layer]]) && !is.na(layer_size[[layer]])) 
          layers[[layer]]$tiles$layer_size <- layer_size[[layer]]
        if (!is.null(simplification_zoom[[layer]]) && 
            !is.na(simplification_zoom[[layer]])) {
          layers[[layer]]$features$simplification[[1]] <- "case"
          layers[[layer]]$features$simplification[[2]] <- 
            list("==", "zoom", simplification_zoom[[layer]])
          layers[[layer]]$features$simplification[[3]] <- 1
          layers[[layer]]$features$simplification[[4]] <- 4
        }
        if (!is.null(bbox)) layers[[layer]]$tiles$bbox <- bbox
        layers
      }) |> reduce(c)
    } else {
      layers[[layer_names]]$source <- source
      layers[[layer_names]]$minzoom <- minzoom
      layers[[layer_names]]$maxzoom <- maxzoom
      if (!is.null(layer_size) && !is.na(layer_size)) 
        layers[[layer_names]]$tiles$layer_size <- layer_size
      if (!is.null(simplification_zoom) && !is.na(simplification_zoom)) {
        layers[[layer_names]]$features$simplification[[1]] <- "case"
        layers[[layer_names]]$features$simplification[[2]] <- 
          list("==", "zoom", simplification_zoom)
        layers[[layer_names]]$features$simplification[[3]] <- 1
        layers[[layer_names]]$features$simplification[[4]] <- 4
      }
      if (!is.null(bbox)) layers[[layer_names]]$tiles$bbox <- bbox
      layers
    }
  
  out <- jsonlite::toJSON(out, pretty = TRUE, auto_unbox = TRUE)
  
  out <- 
    str_replace_all(paste0(out), paste0('\"simplification\": \\[\n            ',
                                        '\"case\",\n            \\[\n         ',
                                        '     \"==\",\n              \"zoom\",'),
                    paste0('\"simplification\": \\[\n            \"case\",\n  ',
                           '          \\[\n              \"==\",\n            ',
                           '  [ \"zoom\" ],')) |> 
    jsonlite::prettify()
  
  out
  
}

# example_recipe_1 <- 
#   create_recipe(layer_names = "DA", 
#                 source = "mapbox://tileset-source/maxbdb2/place_explorer-DA2", 
#                 minzoom = 12, 
#                 maxzoom = 14, 
#                 layer_size = 2500, 
#                 simplification_zoom = 12,
#                 bbox = c(-73.57, 45.50, -73.56, 45.51),
#                 recipe_name = "test17")

# example_recipe_2 <- 
#   to_send <- 
#   create_recipe(layer_names = c("DA", "CT"), 
#                 source = c("DA" = "mapbox://tileset-source/maxbdb2/place_explorer-DA2",
#                            "CT" = "mapbox://tileset-source/maxbdb2/place_explorer-CT2"), 
#                 minzoom = c("DA" = 12,
#                             "CT" = 8), 
#                 maxzoom = c("DA" = 14,
#                             "CT" = 12), 
#                 layer_size = c("DA" = 2500,
#                                "CT" = NA), 
#                 simplification_zoom = c("DA" = NA,
#                                         "CT" = 12),
#                 bbox = c(-73.57, 45.50, -73.56, 45.51),
#                 recipe_name = "test17")
  