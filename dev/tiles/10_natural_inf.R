#### NATURAL INFRASTRUCTURE PROCESSING #########################################

library(tidyverse)
library(sf)
library(qs)
natural_inf_tiles_raw <- qread("dev/data/natural_inf_tiles_raw.qs")
natural_inf_tiles <- qread("dev/data/natural_inf_tiles.qs")
source("dev/tiles/_tile_functions.R")


# Process the lists then upload tile source ---------------------------------

map2(names(natural_inf_tiles_raw), natural_inf_tiles_raw, function(cat, cat_data) {
  
  # Start time
  start_time <- Sys.time()
  
  map(c("high", "mid", "low", "vlow"), function(level) {
    
    # delete_tileset_source(paste0("natural_inf-", cat, "_", str_extract(level, ".")),
    #                       "sus-mcgill")
    
    cat_data[[level]] |>
      as_tibble() |>
      dplyr::select(ends_with("_q100"), geometry) |>
      mutate(across(where(is.numeric), as.character)) |>
      st_as_sf() |>
      st_set_agr("constant") |>
      upload_tile_source(paste0("natural_inf-", cat, "_", str_extract(level, ".")), 
                         "sus-mcgill", .sus_token)
  })
  
  # Create recipe
  natural_inf_recipe <-
    create_recipe(
      layer_names = c(paste0("natural_inf-", cat, "_v"),
                      paste0("natural_inf-", cat, "_l"),
                      paste0("natural_inf-", cat, "_m"),
                      paste0("natural_inf-", cat, "_h")),
      source = c(setNames(nm = paste0("natural_inf-", cat, "_v"), 
                          paste0("mapbox://tileset-source/sus-mcgill/natural_inf-", cat, "_v")),
                 setNames(nm = paste0("natural_inf-", cat, "_l"), 
                          paste0("mapbox://tileset-source/sus-mcgill/natural_inf-", cat, "_l")),
                 setNames(nm = paste0("natural_inf-", cat, "_m"), 
                          paste0("mapbox://tileset-source/sus-mcgill/natural_inf-", cat, "_m")),
                 setNames(nm = paste0("natural_inf-", cat, "_h"), 
                          paste0("mapbox://tileset-source/sus-mcgill/natural_inf-", cat, "_h"))),
      minzoom = c(setNames(nm = paste0("natural_inf-", cat, "_v"), 3),
                  setNames(nm = paste0("natural_inf-", cat, "_l"), 9),
                  setNames(nm = paste0("natural_inf-", cat, "_m"), 10), 
                  setNames(nm = paste0("natural_inf-", cat, "_h"), 11)),
      maxzoom = c(setNames(nm = paste0("natural_inf-", cat, "_v"), 8),
                  setNames(nm = paste0("natural_inf-", cat, "_l"), 9),
                  setNames(nm = paste0("natural_inf-", cat, "_m"), 10), 
                  setNames(nm = paste0("natural_inf-", cat, "_h"), 13)),
      layer_size = c(setNames(nm = paste0("natural_inf-", cat, "_v"), 2500),
                     setNames(nm = paste0("natural_inf-", cat, "_l"), 2500),
                     setNames(nm = paste0("natural_inf-", cat, "_m"), 2500), 
                     setNames(nm = paste0("natural_inf-", cat, "_h"), 2500)),
      simp_zoom = c(setNames(nm = paste0("natural_inf-", cat, "_v"), 8),
                    setNames(nm = paste0("natural_inf-", cat, "_l"), 9),
                    setNames(nm = paste0("natural_inf-", cat, "_m"), 10), 
                    setNames(nm = paste0("natural_inf-", cat, "_h"), NA)),
      simp_value = c(setNames(nm = paste0("natural_inf-", cat, "_v"), 1),
                     setNames(nm = paste0("natural_inf-", cat, "_l"), 1),
                     setNames(nm = paste0("natural_inf-", cat, "_m"), 1), 
                     setNames(nm = paste0("natural_inf-", cat, "_h"), NA)),
      fallback_simp_zoom = c(setNames(nm = paste0("natural_inf-", cat, "_v"), 4),
                             setNames(nm = paste0("natural_inf-", cat, "_l"), 4),
                             setNames(nm = paste0("natural_inf-", cat, "_m"), 4), 
                             setNames(nm = paste0("natural_inf-", cat, "_h"), NA)),
      recipe_name = paste0("natural_inf-", cat))
  

  # Create and publish ------------------------------------------------------
  # Create tileset
  create_tileset(paste0("natural_inf-", cat), natural_inf_recipe)
  
  # Publish tileset
  publish_tileset(paste0("natural_inf-", cat))
  
  # Wait if necessary
  time_dif <- Sys.time() - start_time
  if (time_dif < 31) Sys.sleep(31 - time_dif)

})


# Tileset for custom priorities -------------------------------------------

natural_inf_tiles <- 
  natural_inf_tiles |> 
  mutate(ID = as.character(ID))

iter_size <- ceiling(nrow(natural_inf_tiles) / 100)

union_to_process_list <- 
  map(1:100, ~{
    natural_inf_tiles |> 
      slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
      geojsonsf::sf_geojson() |> 
      paste0(collapse = " ") |> 
      geojson::featurecollection()  
  })

# Iteratively post files to tile source
tmp <- tempfile(fileext = ".json")
tmp_list <- map(1:10, ~tempfile(fileext = ".json"))

map(1:10, ~{

  to_process <- union_to_process_list[((.x - 1) * 10 + 1):(.x * 10)]
  walk2(to_process, tmp_list, geojson::ndgeo_write)
  
  # Concatenate geoJSONs
  out_file <- file(tmp, "w")
  for (i in tmp_list){
    x <- readLines(i)
    writeLines(x, out_file) 
  }
  close(out_file)

  # Upload to MTS
  out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/',
                'sus-mcgill/natural_inf-custom?access_token=', .sus_token,
                '" -F file=@', tmp,
                ' --header "Content-Type: multipart/form-data"')
  system(out)
  
})

natural_inf_recipe <-
  create_recipe(
    layer_names = "natural_inf-custom",
    source = paste0("mapbox://tileset-source/sus-mcgill/natural_inf-custom"),
    minzoom = 3,
    maxzoom = 13,
    layer_size = 2500,
    recipe_name = "natural_inf-custom")

create_tileset("natural_inf-custom", natural_inf_recipe)
publish_tileset("natural_inf-custom")
