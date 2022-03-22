#### EMPTY GEOMETRY TILES ######################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
grid <- qread("data/grid.qs")
building <- qread("data/building.qs")
variables <- qread("data/variables.qs")
source("dev/tiles/tile_functions.R")

# Process DA_empty then upload tile source ---------------------------

DA |> 
  select(ID, name, geometry) |> 
  upload_tile_source("DA_empty")
