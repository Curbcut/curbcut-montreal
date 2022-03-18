#### CANALE TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
source("dev/tiles/tile_functions.R")


# Process CMA then upload tile source -------------------------------------

borough |> 
  select(name) |> 
  st_set_agr("constant") |> 
  st_centroid() |> 
  upload_tile_source("label2", "maxbdb2", .sus_token)


# Add recipes -------------------------------------------------------------

recipe_label <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "label": {
        "source": "mapbox://tileset-source/maxbdb2/label2",
        "minzoom": 3,
        "maxzoom": 11
      }
    }
  },
  "name": "label2"
}
'


# Create and publish tilesets ---------------------------------------------

resp <- create_tileset("label2", recipe_label, "maxbdb2", .sus_token)
httr::content(resp)
publish_tileset("label2", "maxbdb2", .sus_token)
