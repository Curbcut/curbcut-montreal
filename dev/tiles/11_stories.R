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
  mutate(fill = case_when(Type == "Proposed extension" ~ "#1263A6",
                          Type == "Proposed extensions" ~ "#1263A6",
                          Type == "Proposed line" ~ "#000000",
                          Type == "Proposed lines" ~ "#000000",
                          Type == "Proposed orange and green lines" ~ "#000000", 
                          Type == "Proposed red line" ~ "#8B0000",
                          Type == "Green line" ~ "#00A650", 
                          Type == "Orange line" ~ "#F47216", 
                          Type == "Yellow line" ~ "#FCD300",
                          Type == "Proposed orange line extension" ~ "#f5D7A4",
                          Type == "Proposed green line extension" ~ "#76C274",
                          Type == "Proposed blue line" ~ "#9CC0F0",
                          Type == "Blue line" ~ "#1082CD",
                          Type == "Proposed blue line extension" ~ "#9CC0F0",
                          Type == "Proposed line 10" ~ "#000000",
                          Type == "Proposed line 11" ~ "#454545",
                          Type == "Proposed line 6" ~ "#696868",
                          Type == "Proposed line 7" ~ "#949292",
                          Type == "Proposed line 8" ~ "#B5B1B1",
                          Type == "Blue line extension" ~ "#9CC0F0",
                          Type == "Proposed yellow line extension" ~ "#FAF093",
                          Type == "Proposed pink line" ~ "#E60E70")) |> 
  select(date, fill)

metro_evolution <- 
  map_dfr(unique(metro_evolution$date), ~{
    out <- 
      metro_evolution |> 
      filter(date == .x) |> 
      mutate(new_fill = fill)
    
    names(out)[names(out) == "new_fill"] <- paste0("fill_", .x)
    
    out
  }) |> 
  relocate(geometry, .after = last_col()) |> 
  mutate(across(fill_1910:fill_2000, ~{ifelse(is.na(.x), "#FFFFFF00", .x)})) |> 
  select(-date, -fill)

metro_evolution |> 
  upload_tile_source("stories-metro_evolution")

stories_recipe <- 
  create_recipe(
    layer_names = "stories-metro_evolution",
    source = "mapbox://tileset-source/sus-mcgill/stories-metro_evolution",
    minzoom = 3,
    maxzoom = 13, 
    recipe_name = "stories-metro_evolution")

create_tileset("stories-metro_evolution", stories_recipe)
publish_tileset("stories-metro_evolution")

