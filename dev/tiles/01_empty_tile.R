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


# Process DA_empty then upload tile source ---------------------------

DA |> 
  select(ID, geometry) |> 
  upload_tile_source("DA_empty")


# Process DA_empty_island then upload tile source -------------------------

DA |> 
  filter(CSDUID %in% island_CSDUID) |> 
  select(ID, geometry) |> 
  upload_tile_source("DA_empty_island")


# Load and process DA_building_empty data ---------------------------------

DA |> 
  st_set_geometry("building") |> 
  select(ID, name, geometry = building) |> 
  upload_tile_source("DA_building_empty", "sus-mcgill", .sus_token)


# Load and process DA_building_empty_island data --------------------------

DA |> 
  filter(CSDUID %in% island_CSDUID) |> 
  st_set_geometry("building") |> 
  select(ID, name, geometry = building) |> 
  upload_tile_source("DA_building_empty_island", "sus-mcgill", .sus_token)
