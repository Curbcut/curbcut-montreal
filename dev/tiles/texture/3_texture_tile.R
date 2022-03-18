#### CANALE TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/tile_functions.R")

# Texture recipe -----------------------------------------------------------

recipe_texture <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "streets": {
        "source": "mapbox://tileset-source/maxbdb2/streets",
        "minzoom": 12,
        "maxzoom": 16,
        "tiles": {
          "bbox": [ -73.63, 45.45, -73.56, 45.51 ]
        },
      "features": {
        "attributes": {
          "set": {
            "quality": [
              "match",
              [ "get", "group" ],
              "low", 12,
              "mid", 13,
              "high", 14,
              14
            ]
          }
        },
        "filter": [ "<=", [ "get", "quality" ], [ "zoom" ] ]
      }
      },
      "parks": {
        "source": "mapbox://tileset-source/maxbdb2/parks",
        "minzoom": 13,
        "maxzoom": 16,
        "tiles": {
          "bbox": [ -73.63, 45.45, -73.56, 45.51 ]
        }
      }
    }
  },
  "name": "texture13"
}
'

# ,
# "buildings": {
#   "source": "mapbox://tileset-source/maxbdb2/buildings",
#   "minzoom": 16,
#   "maxzoom": 16,
#   "tiles": {
#     "bbox": [ -73.57, 45.50, -73.56, 45.51 ]
#   }
# }


# Create and publish tilesets ---------------------------------------------

resp <- create_tileset("texture13", recipe_texture, "maxbdb2", .sus_token)
httr::content(resp)
resp <- publish_tileset("texture13", "maxbdb2", .sus_token)
httr::content(resp)
