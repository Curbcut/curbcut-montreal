#### VULNERABLE POPULATION PROCESSING #########################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/_tile_functions.R")
# source("R/functions/_utils.R")
qload("data/colours.qsm")
qload("data2/census_full.qsm")
centraide <- qread("data2/centraide_full.qs")

CT <- CT_full

# Univariate CT tileset source --------------------------------------------

CT |> 
  select(ID, contains("_q5") & starts_with("housing_characteristics")) |> 
  rename_with(~str_remove(.x, ("_q5")), everything()) |> 
  mutate(across(where(is.numeric), as.character)) |> 
  st_set_agr("constant") |>
  upload_tile_source("housing_charact-CT",
                     access_token = .sus_token)


# Univariate CT recipe ----------------------------------------------------

recipe_CT <- 
  create_recipe(
    layer_names = "CT",
    source = "mapbox://tileset-source/sus-mcgill/housing_charact-CT",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "housing_charact-CT")


# Create and CT publish univariate ----------------------------------------

# Create tileset
create_tileset("housing_charact-CT", recipe_CT)

# Publish tileset
publish_tileset("housing_charact-CT")



# Univariate centraide tileset source -------------------------------------

centraide |> 
  select(ID, contains("_q5") & starts_with("housing_characteristics")) |> 
  rename_with(~str_remove(.x, ("_q5")), everything()) |> 
  mutate(across(where(is.numeric), as.character)) |> 
  st_set_agr("constant") |>
  upload_tile_source("housing_charact-centraide",
                     access_token = .sus_token)


# Univariate centraide recipe ---------------------------------------------

recipe_centraide <- 
  create_recipe(
    layer_names = "centraide",
    source = "mapbox://tileset-source/sus-mcgill/housing_charact-centraide",
    minzoom = 3,
    maxzoom = 11, 
    simp_zoom = 11,
    layer_size = 2500,
    recipe_name = "housing_charact-centraide")


# Create and publish centraide univariate ---------------------------------

# Create tileset
create_tileset("housing_charact-centraide", recipe_centraide)

# Publish tileset
publish_tileset("housing_charact-centraide")



# Auto-zoom recipes, create and publish -----------------------------------

auto_zoom_recipe <- 
  create_recipe(
    layer_names = c("centraide", "CT"),
    source = c(
      centraide = "mapbox://tileset-source/sus-mcgill/housing_charact-centraide",
      CT = "mapbox://tileset-source/sus-mcgill/housing_charact-CT"),
    minzoom = c(centraide = 2, CT = 11),
    maxzoom = c(centraide = 10, CT = 12), 
    layer_size = c(centraide = NA, CT = NA),
    recipe_name = "housing_charact-auto_zoom")

# Create tileset
create_tileset("housing_charact-auto_zoom", auto_zoom_recipe)

# Publish tileset
publish_tileset("housing_charact-auto_zoom")

