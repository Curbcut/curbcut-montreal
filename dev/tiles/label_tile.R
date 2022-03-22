#### CANALE TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
source("dev/tiles/tile_functions.R")


# Process label then upload tile source -----------------------------------

borough |> 
  select(name, population) |> 
  mutate(name = stringi::stri_trans_general(name, id = "Latin-ASCII")) |> 
  st_set_agr("constant") |> 
  st_centroid() |>
  upload_tile_source("borough_label")


# Add recipe --------------------------------------------------------------

recipe_label <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "label": {
        "source": "mapbox://tileset-source/sus-mcgill/borough_label",
        "minzoom": 9,
        "maxzoom": 14,
        "tiles": {
          "limit": [
            [ "highest_where_in_distance", true, 4, "population" ]
          ]
        }
      }
    }
  },
  "name": "borough_label"
}
'

# Create and publish tileset ----------------------------------------------

create_tileset("borough_label", recipe_label)
publish_tileset("borough_label")
