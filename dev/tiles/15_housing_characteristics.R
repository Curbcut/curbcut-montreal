#### VULNERABLE POPULATION PROCESSING #########################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/_tile_functions.R")
# source("R/functions/_utils.R")
qload("data/colours.qsm")
qload("data2/census_full.qsm")

CT <- CT_full

# Univariate tileset source -----------------------------------------------

CT |> 
  select(ID, contains("_q5") & starts_with("housing_characteristics")) |> 
  rename_with(~str_remove(.x, ("_q5")), everything()) |> 
  mutate(across(where(is.numeric), as.character)) |> 
  st_set_agr("constant") |>
  upload_tile_source("housing_characteristics-CT",
                     access_token = .sus_token)


# Univariate recipe -------------------------------------------------------

recipe_CT <- 
  create_recipe(
    layer_names = "CT",
    source = "mapbox://tileset-source/sus-mcgill/housing_characteristics-CT",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "housing_characteristics-CT")


# Create and publish univariate -------------------------------------------

# Create tileset
create_tileset("housing_characteristics-CT", recipe_CT)

# Publish tileset
publish_tileset("housing_characteristics-CT")
