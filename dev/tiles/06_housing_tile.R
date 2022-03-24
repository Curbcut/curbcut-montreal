#### HOUSING TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
grid <- qread("data/grid.qs")
building_full <- qread("data/building_full.qs")
variables <- qread("data/variables.qs")
source("dev/tiles/_tile_functions.R")
