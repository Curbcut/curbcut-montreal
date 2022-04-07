#### STREET/PARK TILE PROCESSING ###############################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/tile_functions.R")
qload("data/census.qsm")


# Load and process street data --------------------------------------------

street <- qread("dev/data/street_network.qs")

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
  st_filter(borough)

street_1 <- 
  street |>
  filter(group == "low") |> 
  group_by(name, group, street_type) |> 
  summarize(.groups = "drop")

upload_tile_source(street_1, "street_1")

street_2 <- 
  street |>
  filter(group == "mid") |> 
  group_by(name, group, street_type) |> 
  summarize(.groups = "drop")

upload_tile_source(street_2, "street_2")

street_3 <- 
  street |>
  filter(group == "high") |> 
  st_transform(32618) |> 
  st_intersection(st_transform(select(borough, ID, geometry), 32618)) |> 
  st_transform(4326) |> 
  group_by(ID, name, group, street_type) |> 
  summarize(.groups = "drop")

street_3 <- 
  street_3 |> 
  group_by(name, group, street_type) |> 
  summarize(.groups = "drop")

upload_tile_source(street_3, "street_3")


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

upload_tile_source(park, "park", "sus-mcgill", .sus_token)


# Create recipes ----------------------------------------------------------

# Street 1
recipe_street_1 <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "street": {
        "source": "mapbox://tileset-source/sus-mcgill/street_1",
        "minzoom": 12,
        "maxzoom": 16,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 16 ], 1, 4 
          ]
        }
      }
    }
  },
  "name": "street_1"
}
'

# Street 2
recipe_street_2 <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "street": {
        "source": "mapbox://tileset-source/sus-mcgill/street_2",
        "minzoom": 13,
        "maxzoom": 16,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 16 ], 1, 4 
          ]
        }
      }
    }
  },
  "name": "street_2"
}
'

# Street 3
recipe_street_3 <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "street": {
        "source": "mapbox://tileset-source/sus-mcgill/street_3",
        "minzoom": 14,
        "maxzoom": 16,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 16 ], 1, 4 
          ]
        }
      },
      "park": {
        "source": "mapbox://tileset-source/sus-mcgill/park",
        "minzoom": 14,
        "maxzoom": 16
      }
    }
  },
  "name": "street_3"
}
'




# Publish tilesets --------------------------------------------------------

create_tileset("street_1", recipe_street_1)
publish_tileset("street_1")

create_tileset("street_2", recipe_street_2)
publish_tileset("street_2")

create_tileset("street_3", recipe_street_3)
publish_tileset("street_3")
