#### NATURAL INFRASTRUCTURE PROCESSING #########################################

library(tidyverse)
library(sf)
library(qs)
natural_infrastructure_tiles <- qread("dev/data/natural_infrastructure_tiles.qs")
natural_infrastructure_tiles_unioned_nis <- 
  qread("dev/data/natural_infrastructure_tiles_unioned_nis.qs")
source("dev/tiles/_tile_functions.R")

# Process the lists then upload tile source ---------------------------------

map2(seq_along(natural_infrastructure_tiles), natural_infrastructure_tiles, ~{
  
  delete_tileset_source(paste0("natural_infrastructure-", .x), username = "maxbdb2")
  
  .y |>
    as_tibble() |>
    dplyr::select(ends_with("_q100"), geometry) |>
    mutate(across(where(is.numeric), as.character)) |>
    st_as_sf() |>
    st_set_agr("constant") |>
    upload_tile_source(paste0("natural_infrastructure-", .x), "maxbdb2", .sus_token)
  
  # Add recipes -------------------------------------------------------------
  
  natural_infrastructure_raster_recipe <-
    create_recipe(
      layer_names = paste0("natural_infrastructure-", .x),
      source = paste0("mapbox://tileset-source/maxbdb2/natural_infrastructure-", .x),
      minzoom = 3,
      maxzoom = 13,
      layer_size = 2500,
      recipe_name = paste0("natural_infrastructure-", .x))


  # Create and publish ------------------------------------------------------

  create_tileset(paste0("natural_infrastructure-", .x),
                 natural_infrastructure_raster_recipe, username = "maxbdb2")
  Sys.sleep(15)
  publish_tileset(paste0("natural_infrastructure-", .x), username = "maxbdb2")
  
})


# Tileset for unique set of prioritization --------------------------------

natural_infrastructure_tiles_unioned_nis <- 
natural_infrastructure_tiles_unioned_nis |> 
  mutate(ID = as.character(ID))

iter_size <- ceiling(nrow(natural_infrastructure_tiles_unioned_nis) / 100)

union_to_process_list <- 
  map(1:100, ~{
    natural_infrastructure_tiles_unioned_nis |> 
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
                'maxbdb2/natural_infrastructure-custom?access_token=', .sus_token,
                '" -F file=@', tmp,
                ' --header "Content-Type: multipart/form-data"')
  system(out)
  
})

natural_infrastructure_raster_recipe <-
  create_recipe(
    layer_names = "natural_infrastructure-custom",
    source = paste0("mapbox://tileset-source/maxbdb2/natural_infrastructure-custom"),
    minzoom = 3,
    maxzoom = 13,
    layer_size = 2500,
    recipe_name = "natural_infrastructure-custom")

create_tileset("natural_infrastructure-custom",
               natural_infrastructure_raster_recipe, username = "maxbdb2")
Sys.sleep(1)
publish_tileset("natural_infrastructure-custom", username = "maxbdb2")
