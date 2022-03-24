#### HOUSING TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
building_full <- qread("data/building_full.qs")
variables <- qread("data/variables.qs")
source("dev/tiles/_tile_functions.R")


# Get variables to add ----------------------------------------------------

left_vars <-
  variables |> 
  filter(theme == "Housing") |> 
  pull(var_code)

left_vars_census <- paste0(left_vars, "_q5")
left_vars_census_bivar <- paste0(left_vars, "_q3")

right_vars <-
  variables |> 
  filter(source == "census") |> 
  filter(!theme %in% c("Employment", "Housing"), !is.na(theme)) |> 
  pull(var_code) |> 
  paste0("_q3_2016")


# Construct tile lookup table ---------------------------------------------

tile_lookup <- tibble(
  module = "climate_risk",
  tile2 = left_vars,
  suffix = paste0("-", 1:5)
)

qsave(tile_lookup, "data/tile_lookup.qs")

