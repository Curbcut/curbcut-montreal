#### CENTRAIDE POPULATIONS PROCESSING #######################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/_tile_functions.R")
# source("R/functions/_utils.R")
qload("data/colours.qsm")
qload("data2/census_full.qsm")
centraide <- qread("data2/centraide_full.qs")

CT <- CT_full
borough <- borough_full


# Empty CMA CT tileset ----------------------------------------------

CT |> 
  filter(CMA) |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_d-CT",
                     access_token = .sus_token)

# Recipe
recipe_CT <- 
  create_recipe(
    layer_names = "CMA-CT",
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


# Empty borough tileset ----------------------------------------------

borough |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_d-borough",
                     access_token = .sus_token)

# Recipe
recipe_borough <- 
  create_recipe(
    layer_names = "CMA-borough",
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


# Empty centraide tileset ----------------------------------------------

centraide |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_d-centraide-centraide",
                     access_token = .sus_token)

# Recipe
recipe_centraide <- 
  create_recipe(
    layer_names = "centraide-centraide",
    source = "mapbox://tileset-source/sus-mcgill/cent_d-centraide-centraide",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "cent_d-centraide-centraide")

# Create tileset
create_tileset("cent_d-centraide-centraide", recipe_centraide)

# Publish tileset
publish_tileset("cent_d-centraide-centraide")


# Empty centraide CT tileset ----------------------------------------------

CT |> 
  filter(centraide) |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_d-centraide-CT",
                     access_token = .sus_token)

# Recipe
recipe_CT <- 
  create_recipe(
    layer_names = "centraide-CT",
    source = "mapbox://tileset-source/sus-mcgill/cent_d-centraide-CT",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "cent_d-centraide-CT")

# Create tileset
create_tileset("cent_d-centraide-CT", recipe_CT)

# Publish tileset
publish_tileset("cent_d-centraide-CT")


# Centraide auto-zoom -------------------------------------------------------

auto_zoom_recipe <- 
  create_recipe(
    layer_names = c("centraide", "CT"),
    source = c(
      centraide = "mapbox://tileset-source/sus-mcgill/cent_d-centraide-centraide",
      CT = "mapbox://tileset-source/sus-mcgill/cent_d-centraide-CT"),
    minzoom = c(centraide = 2, CT = 11),
    maxzoom = c(centraide = 10, CT = 12), 
    layer_size = c(centraide = NA, CT = NA),
    recipe_name = "cent_d-centraide-auto_zoom")

# Create tileset
create_tileset("cent_d-centraide-auto_zoom", auto_zoom_recipe)

# Publish tileset
publish_tileset("cent_d-centraide-auto_zoom")





