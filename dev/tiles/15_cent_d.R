#### CENTRAIDE DWELLINGS PROCESSING #########################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/_tile_functions.R")
# source("R/functions/_utils.R")
qload("data/colours.qsm")
qload("data2/census_full.qsm")
# centraide <- qread("data2/centraide_full.qs")

CT <- CT_full
borough <- borough_full


# Univariate CT tileset ---------------------------------------------------

CT |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_d-CT",
                     access_token = .sus_token)

# Recipe
recipe_CT <- 
  create_recipe(
    layer_names = "CT",
    source = "mapbox://tileset-source/sus-mcgill/cent_d-CT",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "cent_d-CT")

# Create tileset
create_tileset("cent_d-CT", recipe_CT)

# Publish tileset
publish_tileset("cent_d-CT")


# Univariate borough tileset ----------------------------------------------

borough |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_d-borough",
                     access_token = .sus_token)

# Recipe
recipe_borough <- 
  create_recipe(
    layer_names = "borough",
    source = "mapbox://tileset-source/sus-mcgill/cent_d-borough",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "cent_d-borough")

# Create tileset
create_tileset("cent_d-borough", recipe_borough)

# Publish tileset
publish_tileset("cent_d-borough")


# Census auto-zoom ---------------------------------------------------------

auto_zoom_recipe <- 
  create_recipe(
    layer_names = c("borough", "CT"),
    source = c(
      borough = "mapbox://tileset-source/sus-mcgill/cent_d-borough",
      CT = "mapbox://tileset-source/sus-mcgill/cent_d-CT"),
    minzoom = c(borough = 2, CT = 11),
    maxzoom = c(borough = 10, CT = 12), 
    layer_size = c(borough = NA, CT = NA),
    recipe_name = "cent_d-auto_zoom")

# Create tileset
create_tileset("cent_d-auto_zoom", auto_zoom_recipe)

# Publish tileset
publish_tileset("cent_d-auto_zoom")









# Univariate centraide tileset --------------------------------------------

centraide |> 
  select(ID, contains("_q5") & starts_with("housing_char")) |> 
  rename_with(~str_remove(.x, ("_q5")), everything()) |> 
  mutate(across(where(is.numeric), as.character)) |> 
  st_set_agr("constant") |>
  upload_tile_source("housing_char-centraide",
                     access_token = .sus_token)

# Recipe
recipe_centraide <- 
  create_recipe(
    layer_names = "centraide",
    source = "mapbox://tileset-source/sus-mcgill/housing_char-centraide",
    minzoom = 3,
    maxzoom = 11, 
    simp_zoom = 11,
    layer_size = 2500,
    recipe_name = "housing_char-centraide")

# Create tileset
create_tileset("housing_char-centraide", recipe_centraide)

# Publish tileset
publish_tileset("housing_char-centraide")




# Centraide auto-zoom -----------------------------------------------------

auto_zoom_recipe <- 
  create_recipe(
    layer_names = c("centraide", "CT"),
    source = c(
      centraide = "mapbox://tileset-source/sus-mcgill/housing_char-centraide",
      CT = "mapbox://tileset-source/sus-mcgill/housing_char-CT"),
    minzoom = c(centraide = 2, CT = 11),
    maxzoom = c(centraide = 10, CT = 12), 
    layer_size = c(centraide = NA, CT = NA),
    recipe_name = "housing_char-auto_zoom")

# Create tileset
create_tileset("housing_char-centraide-auto_zoom", auto_zoom_recipe)

# Publish tileset
publish_tileset("housing_char-centraide-auto_zoom")
