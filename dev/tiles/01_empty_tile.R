#### EMPTY GEOMETRY TILES ######################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
grid <- qread("data/grid.qs")
building <- qread("data/building.qs")
variables <- qread("data/variables.qs")
source("dev/tiles/tile_functions.R")

island_CSDUID <- 
  c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
    "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
    "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
    "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
    "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
    "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
    "2466142", "2466072", "2466023")


# Process DA_empty then upload tile source --------------------------------

DA |> 
  select(ID, geometry) |> 
  upload_tile_source("DA_empty")


# Create and publish DA_empty tileset -------------------------------------

DA_empty_recipe <- 
  create_recipe(
    layer_names = "DA",
    source = "mapbox://tileset-source/sus-mcgill/DA_empty",
    minzoom = 8,
    maxzoom = 13, 
    recipe_name = "building_empty")

create_tileset("DA_empty", DA_empty_recipe)
publish_tileset("DA_empty")


# Process DA_empty_island then upload tile source -------------------------

DA |> 
  filter(CSDUID %in% island_CSDUID) |> 
  select(ID, geometry) |> 
  upload_tile_source("DA_empty_island")


# Load and process DA_building_empty data ---------------------------------

DA |> 
  st_set_geometry("building") |> 
  select(ID, name, geometry = building) |> 
  upload_tile_source("DA_building_empty")


# Load and process DA_building_empty_island data --------------------------

DA |> 
  filter(CSDUID %in% island_CSDUID) |> 
  st_set_geometry("building") |> 
  select(ID, name, geometry = building) |> 
  upload_tile_source("DA_building_empty_island")


# Load and process building_empty -----------------------------------------

building_to_process <- 
  building_full |> 
  select(ID, name, geometry)

iter_size <- ceiling(nrow(building_to_process) / 100)

building_to_process_list <- 
  map(1:100, ~{
    building_to_process |> 
      slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
      geojsonsf::sf_geojson() |> 
      paste0(collapse = " ") |> 
      geojson::featurecollection()  
  })

# Iteratively post files to tile source
tmp <- tempfile(fileext = ".json")
tmp_list <- map(1:10, ~tempfile(fileext = ".json"))

map(1:10, ~{
  
  print(.x)
  to_process <- building_to_process_list[((.x - 1) * 10 + 1):(.x * 10)]
  walk2(to_process, tmp_list, geojson::ndgeo_write)
  
  # Concatenate geoJSONs
  out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
  system(out)
  
  # Upload to MTS
  out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                'sus-mcgill/building_empty?access_token=', .sus_token, 
                '" -F file=@', tmp, 
                ' --header "Content-Type: multipart/form-data"')
  system(out)
  
})


# Create and publish building_empty tileset -------------------------------

building_recipe <- 
  create_recipe(
  layer_names = "building",
  source = "mapbox://tileset-source/sus-mcgill/building_empty",
  minzoom = 14,
  maxzoom = 14, 
  layer_size = 2500,
  recipe_name = "building_empty")

create_tileset("building_empty", building_recipe)
publish_tileset("building_empty")
