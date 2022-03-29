#### PLACE EXPLORER TILE PROCESSING ############################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
source("dev/tiles/tile_functions.R")

# Process empty DA then upload tile source --------------------------------

DA |> 
  select(DAUID, CTUID, CSDUID) |> 
  st_set_agr("constant") |> 
  upload_tile_source("place_explorer-DA", .sus_token)


# Add recipes -------------------------------------------------------------

recipe_DA <- 
  create_recipe(
    layer_names = "DA",
    source = "mapbox://tileset-source/sus-mcgill/place_explorer-DA",
    minzoom = 8,
    maxzoom = 13, 
    recipe_name = "place_explorer-DA")

# Create and publish tilesets ---------------------------------------------

create_tileset("place_explorer-DA", recipe_DA)
publish_tileset("place_explorer-DA")
