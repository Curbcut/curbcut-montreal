#### CANALE TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
natural_infrastructure_tiles <- qread("dev/data/natural_infrastructure_tiles.qs")
source("dev/tiles/_tile_functions.R")


# Get variables to add ----------------------------------------------------

vars_to_add <- 
  c("habitat_quality", "habitat_connectivity", 
    "favorable_climatic_conditions", "ni_contribution_flood_prevention", 
    "ni_contribution_biodiversity_conservation",
    "ni_contribution_heat_island_reduction", "conservation_prioritization") |> 
  paste0("_q100")


# # Process the lists then upload tile source ---------------------------------

map2(seq_along(natural_infrastructure_tiles), natural_infrastructure_tiles, ~{
  
  .y |>
    as_tibble() |>
    dplyr::select(any_of(vars_to_add), geometry) |>
    mutate(across(where(is.numeric), as.character)) |>
    st_as_sf() |>
    st_set_agr("constant") |>
    upload_tile_source(paste0("natural_infrastructure-", .x), "maxbdb2", .sus_token)
  
  # Add recipes -------------------------------------------------------------
  
  natural_infrastructure_raster_recipe <-
    create_recipe(
      layer_names = paste0("natural_infrastructure-", .x),
      source = paste0("mapbox://tileset-source/maxbdb2/natural_infrastructure-", .x),
      minzoom = 3,
      maxzoom = 13,
      layer_size = 2500,
      recipe_name = paste0("natural_infrastructure-", .x))


  # Create and publish ------------------------------------------------------

  create_tileset(paste0("natural_infrastructure-", .x),
                 natural_infrastructure_raster_recipe, username = "maxbdb2")
  Sys.sleep(10)
  publish_tileset(paste0("natural_infrastructure-", .x), username = "maxbdb2")
  
})
