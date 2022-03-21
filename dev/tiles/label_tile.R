#### CANALE TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
source("dev/tiles/tile_functions.R")


# Process CMA then upload tile source -------------------------------------

borough |> 
  select(name, population) |> 
  mutate(name = stringi::stri_trans_general(name, id = "Latin-ASCII")) |> 
  st_set_agr("constant") |> 
  st_centroid() |> View()
  upload_tile_source("label4", "maxbdb2", .sus_token)


# Add recipes -------------------------------------------------------------

recipe_label <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "label": {
        "source": "mapbox://tileset-source/maxbdb2/label4",
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
  "name": "label7"
}
'

# Create and publish tilesets ---------------------------------------------

resp <- create_tileset("label7", recipe_label, "maxbdb2", .sus_token)
httr::content(resp)
publish_tileset("label7", "maxbdb2", .sus_token)
