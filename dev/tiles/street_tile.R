#### STREET/BUILDING TILE PROCESSING ###########################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/tile_functions.R")
qload("data/census.qsm")


# Load and process empty building data ------------------------------------

DA |> 
  st_set_geometry("building") |> 
  select(ID, name, geometry = building) |> 
  upload_tile_source("DA_building_empty", "sus-mcgill", .sus_token)


# Load and process street data --------------------------------------------

street <- qread("dev/data/street_network.qs")
qload("data/census.qsm")

street <- 
  street |> 
  filter(highway %in% c("motorway", "trunk", "primary", "secondary", "tertiary",
                        "motorway_link", "trunk_link", "primary_link", 
                        "residential", "service")) |> 
  mutate(group = case_when(
    highway %in% c("motorway", "trunk", "primary") ~ "low",
    highway %in% c("secondary", "tertiary", "motorway_link", "trunk_link",
                   "primary_link", "residential") ~ "mid",
    TRUE ~ "high")) |>
  select(name, group, street_type = highway) |> 
  st_transform(32618) |> 
  st_intersection(st_transform(select(borough, ID, geometry), 32618)) |> 
  st_transform(4326)

street <- 
  street |> 
  group_by(ID, name, group, street_type) |> 
  summarize(.groups = "drop")
  
street |>
  select(-ID) |> 
  upload_tile_source("street_empty", "sus-mcgill", .sus_token)


# Load and process park data ----------------------------------------------

suppressPackageStartupMessages({library(osmdata)})
CMA_mtl_bb <- c(-74.32797, 45.21754, -73.12856, 45.96849)

park <- 
  CMA_mtl_bb |> 
  opq(timeout = 200) |> 
  add_osm_feature(key = "leisure") |> 
  osmdata_sf() |>
  pluck("osm_polygons") |>
  st_cast("POLYGON") |>
  as_tibble() |>
  st_as_sf() |>
  st_transform(32618) |> 
  st_filter(st_transform(borough, 32618)) |> 
  st_transform(4326) |> 
  st_set_agr("constant") |> 
  filter(leisure != "nature_reserve", !is.na(leisure)) |>
  select(name)

upload_tile_source(park, "park_empty", "sus-mcgill", .sus_token)





# Create recipes ----------------------------------------------------------

# Empty buildings
recipe_building_empty <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "building_empty": {
        "source": "mapbox://tileset-source/sus-mcgill/DA_building_empty",
        "minzoom": 16,
        "maxzoom": 16,
        "tiles": {
          "layer_size": 2500
        }
      }
    }
  },
  "name": "DA_building_empty"
}
'







# Publish tilesets --------------------------------------------------------


create_tileset("building_add", recipe_building_add, "sus-mcgill", .sus_token)
publish_tileset("building_add", "sus-mcgill", .sus_token)








# Create recipe -----------------------------------------------------------

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
  
