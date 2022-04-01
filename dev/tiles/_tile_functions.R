#### MTS FUNCTIONS #############################################################


# List tile sources -------------------------------------------------------

list_tile_sources <- function(username = "sus-mcgill", 
                              access_token = .sus_token) {
  
  httr::GET(paste0("https://api.mapbox.com/tilesets/v1/sources/", username),
              query = list(access_token = access_token, limit = 500)) |> 
    httr::content() |> 
    map_dfr(~tibble(
      id = str_remove(.x$id, "mapbox://tileset-source/sus-mcgill/"),
      size = .x$size / 1024 ^ 2,
      files = .x$files))
  
}

# Upload tile source ------------------------------------------------------

upload_tile_source <- function(df, id, username = "sus-mcgill", 
                               access_token = .sus_token) {
  
  # Initialize tempfile
  tmp1 <- tempfile(fileext = ".json")
  tmp2 <- tempfile(fileext = ".geojson")
  
  # Write Geojson to tempfile
  geojsonio::geojson_write(df, file = tmp2)
  
  suppressWarnings(readtext::readtext(tmp2)) |> 
    paste0(collapse = " ") |> 
    geojson::featurecollection() |> 
    geojson::ndgeo_write(tmp1)
  
  # Construct system call
  out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                username, '/', id, '?access_token=', access_token, 
                '" -F file=@', tmp1, 
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
            query = list(access_token = access_token, limit = 500)) |> 
  httr::content() |> 
    map_dfr(~tibble(
      id = str_remove(.x$id, "sus-mcgill."),
      size = .x$filesize / 1024 ^ 2,
      precisions = paste(names(.x$tileset_precisions), collapse = ", ")
    ))
  
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
                          fallback_simpplification_zoom = 4,
                          features_simplification = NULL, bbox = NULL, 
                          recipe_name) {
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
        layers[[layer_names]]$features$simplification[[4]] <- 
          fallback_simpplification_zoom
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


# Test data before upload -------------------------------------------------

test_data_to_upload <- function(df, new_data, left_vars, right_vars) {
  
  source("R/functions/_get_data.R")
  source("R/functions/_get_data_type.R")
  source("R/functions/_get_data_table.R")
  
  new_data <- st_drop_geometry(new_data)
  
  walk(left_vars, function(var_left) {
    map(right_vars, function(var_right) {
      
      new_data <- {
        if (var_right == " ") return(new_data[, var_left])
        new_data[, paste0(var_left, "_", var_right)]
      }
      
      get_data_fill <- 
        get_data(df, var_left, var_right) |> 
        select(ID, group) |> 
        left_join(colour_bivar, by = "group") |> 
        pull(fill)
      
      new_data <- 
        new_data |> 
        rename(group = 1) |> 
        left_join(my_colour_table, by = "group") |> 
        pull(value)
      
      sum(get_data_fill == new_data) == length(new_data)
      
      if (!sum(get_data_fill == new_data) == length(new_data)) {
        stop(paste0(left_var, " and ", var_left, " have the wrong trans_var")) 
      }
    })
    
  })
  
}
