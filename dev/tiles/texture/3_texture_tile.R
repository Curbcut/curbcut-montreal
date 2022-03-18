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
        "source": "mapbox://tileset-source/maxbdb2/streets2",
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
        "minzoom": 14,
        "maxzoom": 16,
        "tiles": {
          "bbox": [ -73.63, 45.45, -73.56, 45.51 ]
        }
      }
    }
  },
  "name": "texture16"
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

resp <- create_tileset("texture16", recipe_texture, "maxbdb2", .sus_token)
httr::content(resp)
resp <- publish_tileset("texture16", "maxbdb2", .sus_token)
httr::content(resp)

# Three different tiles ---------------------------------------------------
# 
# recipe_street_low <- '
# {
#   "recipe": {
#     "version": 1,
#     "layers": {
#       "streets": {
#         "source": "mapbox://tileset-source/maxbdb2/street-low2",
#         "minzoom": 12,
#         "maxzoom": 16,
#         "tiles": {
#           "bbox": [ -73.63, 45.45, -73.56, 45.51 ]
#         }
#       }
#     }
#   },
#   "name": "street-low2"
# }
# '
# 
# recipe_street_mid <- '
# {
#   "recipe": {
#     "version": 1,
#     "layers": {
#       "streets": {
#         "source": "mapbox://tileset-source/maxbdb2/street-mid3",
#         "minzoom": 13,
#         "maxzoom": 16,
#         "tiles": {
#           "bbox": [ -73.63, 45.45, -73.56, 45.51 ]
#         }
#       }
#     }
#   },
#   "name": "street-mid3"
# }
# '
# 
# recipe_street_high <- '
# {
#   "recipe": {
#     "version": 1,
#     "layers": {
#       "streets": {
#         "source": "mapbox://tileset-source/maxbdb2/street-high3",
#         "minzoom": 14,
#         "maxzoom": 16,
#         "tiles": {
#           "bbox": [ -73.63, 45.45, -73.56, 45.51 ]
#         }
#       }
#     }
#   },
#   "name": "street-high3"
# }
# '
# 
# resp <- create_tileset("street-low2", recipe_street_low, "maxbdb2", .sus_token)
# httr::content(resp)
# resp <- publish_tileset("street-low2", "maxbdb2", .sus_token)
# httr::content(resp)
# 
# resp <- create_tileset("street-mid3", recipe_street_mid, "maxbdb2", .sus_token)
# httr::content(resp)
# resp <- publish_tileset("street-mid3", "maxbdb2", .sus_token)
# httr::content(resp)
# 
# resp <- create_tileset("street-high3", recipe_street_high, "maxbdb2", .sus_token)
# httr::content(resp)
# resp <- publish_tileset("street-high3", "maxbdb2", .sus_token)
# httr::content(resp)
