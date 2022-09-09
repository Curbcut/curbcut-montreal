## UPLOAD ALL EMPTY TILESETS ###################################################

library(tidyverse)
library(sf)
library(qs)
source("dev/tiles/_tile_functions.R")
qload("data2/census_full.qsm")
centraide <- qread("data2/centraide_full.qs")

CT <- CT_full
borough <- borough_full
DA <- DA_full


# Combinations ------------------------------------------------------------

combinations <- 
  list(
    "CMA" = list(
      "borough" = "borough",
      "CT" = "CT",
      "DA" = "DA",
      "auto_zoom" = c("borough", "CT", "DA"),
      "auto_zoom_max_CT" = c("borough", "CT")),
    
    "centraide" = list(
      "centraide" = "centraide",
      "CT" = "CT",
      "DA" = "DA",
      "auto_zoom" = c("centraide", "CT", "DA"),
      "auto_zoom_max_CT" = c("centraide", "CT")))


# Tileset sources ---------------------------------------------------------

imap(combinations, function(scales, geo) {
  map(scales, function(scale) {
    if (length(scale) != 1) return()
    get(scale) |> 
      (\(x) if (scale %in% c("CT", "DA")) filter(x, .data[[geo]]) else x)() |> 
      select(ID) |> 
      st_set_agr("constant") |>
      upload_tile_source(id = paste(geo, scale, sep = "-"),
                         access_token = .sus_token)
    
    print(paste(geo, scale, sep = "-"))
  })
})


# Tileset recipes ---------------------------------------------------------

first_level_recipe_fun <- function(name) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 11, 
    simp_zoom = 11,
    layer_size = 2500,
    recipe_name = name)
}

CT_recipe_fun <- function(name) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = name)
}

DA_recipe_fun <- function(name) {
  create_recipe(
    layer_names = name,
    source = paste0("mapbox://tileset-source/sus-mcgill/", name),
    minzoom = 3,
    maxzoom = 13, 
    simp_zoom = 13,
    layer_size = 2500,
    recipe_name = name)
}

all_recipes <- 
  imap(combinations, function(scales, geo) {
    map2(scales, seq_along(scales), function(scale, level) {
      
      # Auto-zoom cases
      if (length(scale) != 1) {
        warning(
          paste0("Don't forget autozoom for ", paste(geo, sep = "-")))
        return()
      }
      
      scale_fun <- if (level == 1) "first_level" else scale
      do.call(paste0(scale_fun, "_recipe_fun"), 
              list(paste(geo, scale, sep = "-")))
    })
  })

# Tileset recipes for auto-zooms
all_recipes$CMA$auto_zoom <- 
  create_recipe(
    layer_names = c("borough", "CT", "DA"),
    source = c(
      borough = "mapbox://tileset-source/sus-mcgill/CMA-borough",
      CT = "mapbox://tileset-source/sus-mcgill/CMA-CT",
      DA = "mapbox://tileset-source/sus-mcgill/CMA-DA"),
    minzoom = c(borough = 2, CT = 11, DA = 13),
    maxzoom = c(borough = 10, CT = 12, DA = 15), 
    layer_size = c(borough = NA, CT = NA, DA = NA),
    recipe_name = "CMA-auto_zoom")
  
all_recipes$CMA$`auto_zoom_max_CT` <- 
  create_recipe(
    layer_names = c("borough", "CT"),
    source = c(
      borough = "mapbox://tileset-source/sus-mcgill/CMA-borough",
      CT = "mapbox://tileset-source/sus-mcgill/CMA-CT"),
    minzoom = c(borough = 2, CT = 11),
    maxzoom = c(borough = 10, CT = 12), 
    layer_size = c(borough = NA, CT = NA),
    recipe_name = "CMA-auto_zoom_max_CT")

all_recipes$centraide$auto_zoom <- 
  create_recipe(
    layer_names = c("centraide", "CT", "DA"),
    source = c(
      centraide = "mapbox://tileset-source/sus-mcgill/centraide-centraide",
      CT = "mapbox://tileset-source/sus-mcgill/centraide-CT",
      DA = "mapbox://tileset-source/sus-mcgill/centraide-DA"),
    minzoom = c(centraide = 2, CT = 11, DA = 13),
    maxzoom = c(centraide = 10, CT = 12, DA = 15), 
    layer_size = c(centraide = NA, CT = NA, DA = NA),
    recipe_name = "centraide-auto_zoom")
  
all_recipes$centraide$`auto_zoom_max_CT` <- 
  create_recipe(
    layer_names = c("centraide", "CT"),
    source = c(
      centraide = "mapbox://tileset-source/sus-mcgill/centraide-centraide",
      CT = "mapbox://tileset-source/sus-mcgill/centraide-CT"),
    minzoom = c(centraide = 2, CT = 11),
    maxzoom = c(centraide = 10, CT = 12), 
    layer_size = c(centraide = NA, CT = NA),
    recipe_name = "centraide-auto_zoom_max_CT")


if (map(all_recipes, map_lgl, is.null) |> unlist() |> sum() > 0) {
  stop("You need to add a manual recipe for an auto-zoom. You missed one!")
}


# Create tilesets ---------------------------------------------------------

imap(all_recipes, function(recipes, geo) {
  map2(recipes, seq_along(recipes), function(recipe, level) {
    create_tileset(paste(geo, names(recipes)[level], sep = "-"), recipe)
  })
})


# Publish tilesets --------------------------------------------------------

imap(all_recipes, function(recipes, geo) {
  map2(recipes, seq_along(recipes), function(recipe, level) {
    publish_tileset(paste(geo, names(recipes)[level], sep = "-"))
  })
})

### Grid -----------------------------------------------------------------------


