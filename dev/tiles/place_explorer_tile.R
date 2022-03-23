#### PLACE EXPLORER TILE PROCESSING ############################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
source("dev/tiles/tile_functions.R")

# Process DA_building then upload tile source -----------------------------

DA |> 
  select(DAUID, CTUID, CSDUID) |> 
  st_set_agr("constant") |> 
  upload_tile_source("place_explorer-DA2", "maxbdb2", .sus_token)


# Add recipes -------------------------------------------------------------

recipe_DA <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "DA": {
        "source": "mapbox://tileset-source/maxbdb2/place_explorer-DA2",
        "minzoom": 8,
        "maxzoom": 13
      }
    }
  },
  "name": "place_explorer-DA2"
}
'

# Create and publish tilesets ---------------------------------------------

create_tileset("place_explorer-DA2", recipe_DA, "maxbdb2")
publish_tileset("place_explorer-DA2", "maxbdb2")
