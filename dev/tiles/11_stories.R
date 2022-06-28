#### STORIES TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/stories.qsm")
source("dev/tiles/_tile_functions.R")


# Create and upload main map tileset --------------------------------------

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


# Create and upload metro_evolution maps ----------------------------------

metro_evolution <- 
  list.files("dev/data/stories/shp/metro_evolution/", full.names = TRUE) |> 
  str_subset("\\.shp$")

metro_evolution <- 
  map(set_names(metro_evolution), read_sf) |> 
  (\(x) map2(x, names(x), ~{mutate(.x, date = .y)}))() |> 
  reduce(rbind) |> 
  mutate(date = as.character(str_extract(date, "\\d{4}"))) |> 
  rename(type_en = Type) |> 
  mutate(type_fr = case_when(type_en == "Proposed extensions" ~ 
                               "Prolongements proposés",
                             type_en == "Proposed extension" ~ 
                               "Prolongements proposé",
                             type_en == "Proposed line" ~ 
                               "Ligne proposée",
                             type_en == "Proposed lines" ~ 
                               "Ligne proposées",
                             type_en == "Proposed orange and green lines" ~ 
                               "Lignes orange and verte proposées",
                             type_en == "Proposed red line" ~ 
                               "Ligne rouge proposée",
                             type_en == "Proposed extensions" ~ 
                               "Prolongements proposés",
                             type_en == "Green line" ~ 
                               "Ligne verte",
                             type_en == "Orange line" ~ 
                               "Ligne orange",
                             type_en == "Yellow line" ~ 
                               "Ligne jaune",
                             type_en == "Blue line" ~ 
                               "Ligne bleue",
                             type_en == "Proposed orange line extension" ~ 
                               "Prolongement de la ligne orange proposé",
                             type_en == "Proposed green line extension" ~ 
                               "Prolongement de la ligne verte proposé",
                             type_en == "Proposed blue line" ~ 
                               "Ligne bleue proposée",
                             type_en == "Proposed blue line extension" ~ 
                               "Prolongement de la ligne bleue proposé",
                             type_en == "Proposed line 10" ~ 
                               "Ligne 10 proposée",
                             type_en == "Proposed line 11" ~ 
                               "Ligne 11 proposée",
                             type_en == "Proposed line 6" ~ 
                               "Ligne 6 proposée",
                             type_en == "Proposed line 7" ~ 
                               "Ligne 7 proposée",
                             type_en == "Proposed line 8" ~ 
                               "Ligne 8 proposée",
                             type_en == "Blue line extension" ~ 
                               "Prolongement de la ligne bleue",
                             type_en == "Proposed pink line" ~ 
                               "Ligne rose proposée",
                             type_en == "Proposed yellow line extension" ~ 
                               "Prolongement de la ligne jaune proposé")) |> 
  relocate(date, type_en, type_fr) |> 
  rename(`The type` = type_en,
         `Le type` = type_fr)


