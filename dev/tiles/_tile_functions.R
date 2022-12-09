#### MTS FUNCTIONS #############################################################


# List tile sources -------------------------------------------------------

list_tile_sources <- function(username = "sus-mcgill", 
                              access_token = .cc_mb_token) {
  
  res <- httr::GET(paste0("https://api.mapbox.com/tilesets/v1/sources/", 
                          username),
                   query = list(access_token = access_token, limit = 500))
  resDF <- jsonlite::fromJSON(httr::content(res, as = "text"))
  while (isTRUE(grepl("next", res$headers$link))) {
    res <- httr::GET(str_extract(res$headers$link, "(?<=\\<).*(?=>)"),
               query = list(access_token = access_token, limit = 500))
    resDF <- rbind(resDF, jsonlite::fromJSON(httr::content(res, as = "text")))
  }
  
  resDF |> 
    as_tibble() |> 
    mutate(id = str_remove(id, "mapbox://tileset-source/sus-mcgill/"),
           size = size / 1024 ^ 2)
}


# Upload tile source ------------------------------------------------------

upload_tile_source <- function(df, id, username = "sus-mcgill", 
                               access_token = .cc_mb_token) {
  
  # Initialize tempfile
  tmp1 <- tempfile(fileext = ".json")
  tmp2 <- tempfile(fileext = ".geojson")
  
  # Write Geojson to tempfile
  out <- capture.output(capture.output(
    geojsonio::geojson_write(df, file = tmp2), type = "message"))
  
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
                                  access_token = .cc_mb_token) {
  
  out <- httr::DELETE(paste0("https://api.mapbox.com/tilesets/v1/sources/", 
                             username, "/", id), 
                      query = list(access_token = access_token))
  
  if (is_empty(httr::content(out))) return("Success")
  return(httr::content(out))}


# List tilesets -----------------------------------------------------------

list_tilesets <- function(username = "sus-mcgill", access_token = .cc_mb_token) {
  
  res <- httr::GET(paste0("https://api.mapbox.com/tilesets/v1/", 
                          username),
                   query = list(access_token = access_token, limit = 500))
  resDF <- jsonlite::fromJSON(httr::content(res, as = "text")) |> 
    select(id, filesize, starts_with("tileset_precisions"))
  while (isTRUE(grepl("next", res$headers$link))) {
    res <- httr::GET(str_extract(res$headers$link, "(?<=\\<).*(?=>)"),
               query = list(access_token = access_token, limit = 500))
    resDF <- bind_rows(resDF, jsonlite::fromJSON(httr::content(res, as = "text"))) |> 
      select(id, filesize, starts_with("tileset_precisions"))
  }
  
  resDF |> 
    as_tibble() |> 
    transmute(id = str_remove(id, "sus-mcgill."),
              size = filesize / 1024 ^ 2,
              precisions = paste(names(resDF$tileset_precisions), 
                                 collapse = ", "))
  
}

# Create tileset ----------------------------------------------------------

create_tileset <- function(tileset, recipe, username = "sus-mcgill", 
                           access_token = .cc_mb_token) {
  
  # More complex httr::RETRY
  out <- httr::POST(
    url = paste0("https://api.mapbox.com/tilesets/v1/",
                 username, ".", tileset),
    query = list(access_token = access_token),
    body = recipe,
    httr::content_type("application/json")
  )

  if (is_empty(httr::content(out))) return("Success")
  return(httr::content(out))
  
}


# Delete tileset ----------------------------------------------------------

delete_tileset <- function(id, username = "sus-mcgill", 
                           access_token = .cc_mb_token) {
  
  out <- httr::DELETE(paste0("https://api.mapbox.com/tilesets/v1/", username,
                      ".", id), query = list(access_token = access_token))
  
  return(httr::content(out))
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
                            access_token = .cc_mb_token) {
  
  out <- httr::RETRY("POST",
                     url = paste0("https://api.mapbox.com/tilesets/v1/", username, ".", tileset,
                                  "/publish"),
                     query = list(access_token = access_token),
                     times = 5,
                     pause_min = 30
  )
  
  
  if (is_empty(httr::content(out))) return("Success")
  return(httr::content(out))
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
                          layer_size = NULL, simp_zoom = NULL, simp_value = NULL, 
                          fallback_simp_zoom = 4, bbox = NULL, recipe_name) {
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
        if (!is.null(simp_zoom[[layer]]) && 
            !is.na(simp_zoom[[layer]])) {
          layers[[layer]]$features$simplification[[1]] <- "case"
          layers[[layer]]$features$simplification[[2]] <- 
            list("==", "zoom", simp_zoom[[layer]])
          layers[[layer]]$features$simplification[[3]] <- 
            if (!is.null(simp_value[[layer]]) && 
                         !is.na(simp_value[[layer]])) simp_value[[layer]] else 1
          layers[[layer]]$features$simplification[[4]] <- 
            fallback_simp_zoom[[layer]]
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
      if (!is.null(simp_zoom) && !is.na(simp_zoom)) {
        layers[[layer_names]]$features$simplification[[1]] <- "case"
        layers[[layer_names]]$features$simplification[[2]] <- 
          list("==", "zoom", simp_zoom)
        layers[[layer_names]]$features$simplification[[3]] <- 
          if (!is.null(simp_value) && !is.na(simp_value)) simp_value else 1
        layers[[layer_names]]$features$simplification[[4]] <- 
          fallback_simp_zoom
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
        left_join(colour_table, by = "group") |> 
        pull(value)
      
      sum(get_data_fill == new_data) == length(new_data)
      
      if (!sum(get_data_fill == new_data) == length(new_data)) {
        stop(paste0(left_var, " and ", var_left, " have the wrong trans_var")) 
      }
    })
    
  })
  
}
