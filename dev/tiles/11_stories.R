#### STORIES TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/stories.qsm")
source("dev/tiles/_tile_functions.R")


# Create and upload tileset -----------------------------------------------

# Table rase first
delete_tileset_source("stories-stories")
delete_tileset("stories-stories")

stories |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  select(ID, name, geometry) |> 
  upload_tile_source("stories-stories")

stories_recipe <- 
  create_recipe(
    layer_names = "stories-stories",
    source = "mapbox://tileset-source/sus-mcgill/stories-stories",
    minzoom = 3,
    maxzoom = 13, 
    recipe_name = "stories-stories")

create_tileset("stories-stories", stories_recipe)
publish_tileset("stories-stories")
