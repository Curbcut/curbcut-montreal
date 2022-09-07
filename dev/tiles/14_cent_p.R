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


# Univariate CT tileset ---------------------------------------------------

CT |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_p-CT",
                     access_token = .sus_token)

# Recipe
recipe_CT <- 
  create_recipe(
    layer_names = "CT",
    source = "mapbox://tileset-source/sus-mcgill/cent_p-CT",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "cent_p-CT")

# Create tileset
create_tileset("cent_p-CT", recipe_CT)

# Publish tileset
publish_tileset("cent_p-CT")


# Univariate borough tileset ----------------------------------------------

borough |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_p-borough",
                     access_token = .sus_token)

# Recipe
recipe_borough <- 
  create_recipe(
    layer_names = "borough",
    source = "mapbox://tileset-source/sus-mcgill/cent_p-borough",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "cent_p-borough")

# Create tileset
create_tileset("cent_p-borough", recipe_borough)

# Publish tileset
publish_tileset("cent_p-borough")


# Census auto-zoom ---------------------------------------------------------

auto_zoom_recipe <- 
  create_recipe(
    layer_names = c("borough", "CT"),
    source = c(
      borough = "mapbox://tileset-source/sus-mcgill/cent_p-borough",
      CT = "mapbox://tileset-source/sus-mcgill/cent_p-CT"),
    minzoom = c(borough = 2, CT = 11),
    maxzoom = c(borough = 10, CT = 12), 
    layer_size = c(borough = NA, CT = NA),
    recipe_name = "cent_p-auto_zoom")

# Create tileset
create_tileset("cent_p-census_max_CT-auto_zoom", auto_zoom_recipe)

# Publish tileset
publish_tileset("cent_p-census_max_CT-auto_zoom")


# Univariate centraide tileset ----------------------------------------------

centraide |> 
  select(ID) |> 
  st_set_agr("constant") |>
  upload_tile_source("cent_p-centraide",
                     access_token = .sus_token)

# Recipe
recipe_centraide <- 
  create_recipe(
    layer_names = "centraide",
    source = "mapbox://tileset-source/sus-mcgill/cent_p-centraide",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "cent_p-centraide")

# Create tileset
create_tileset("cent_p-centraide", recipe_centraide)

# Publish tileset
publish_tileset("cent_p-centraide")


# Centraide auto-zoom -------------------------------------------------------

auto_zoom_recipe <- 
  create_recipe(
    layer_names = c("centraide", "CT"),
    source = c(
      centraide = "mapbox://tileset-source/sus-mcgill/cent_p-centraide",
      CT = "mapbox://tileset-source/sus-mcgill/cent_p-CT"),
    minzoom = c(centraide = 2, CT = 11),
    maxzoom = c(centraide = 10, CT = 12), 
    layer_size = c(centraide = NA, CT = NA),
    recipe_name = "cent_p-auto_zoom")

# Create tileset
create_tileset("cent_p-centraide-auto_zoom", auto_zoom_recipe)

# Publish tileset
publish_tileset("cent_p-centraide-auto_zoom")





