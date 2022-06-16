#### VULNERABLE POPULATION PROCESSING #########################################

library(tidyverse)
library(sf)
library(qs)
vulnerable_pop <- qread("data2/vulnerable_pop_full.qs")
source("dev/tiles/_tile_functions.R")
# source("R/functions/_utils.R")
qload("data/colours.qsm")


# Calculate q5 ------------------------------------------------------------

pen_col <- names(vulnerable_pop)[length(vulnerable_pop) - 1]

vulnerable_pop |> 
  mutate(across(total_total_total_total:pen_col, ~as.numeric(ntile(.x, 5)))) |> 
  mutate(across(total_total_total_total:pen_col, ~if_else(is.na(.x), 0, .x))) |> 
  st_set_agr("constant") |>
  upload_tile_source(id = "vulnerable_pop-CT",
                     username = "maxbdb3",
                     access_token = .sus_token)


# Create recipe -----------------------------------------------------------

recipe_CT <- 
  create_recipe(
    layer_names = "CT",
    source = "mapbox://tileset-source/maxbdb3/vulnerable_pop-CT",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "vulnerable_pop-CT")


# Create and publish ------------------------------------------------------

# Create tileset
create_tileset("vulnerable_pop-CT",
               username = "maxbdb3", recipe_CT)

# Publish tileset
publish_tileset("vulnerable_pop-CT", 
                username = "maxbdb3")
