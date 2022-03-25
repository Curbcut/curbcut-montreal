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
  filter(theme == "Housing", source == "census") |> 
  pull(var_code)

years <- 
  variables |> 
  filter(theme == "Housing", source == "census") |> 
  pull(dates) |> 
  unlist() |> 
  unique() |> 
  sort()

year_comb <-
  expand.grid(years, years) |> 
  as_tibble() |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), as.numeric)) |> 
  filter(Var1 < Var2) |> 
  as.vector() |> 
  split(1:10) |> 
  map(unlist) |> 
  map(unname)

left_vars |> 
  map(paste, years, sep = "_") |> 
  unlist() |> 
  c(left_vars |> 
      map(\(x) map(year_comb, \(y) paste(x, paste(y, collapse = "_"), 
                                         sep = "_"))) |> 
      unlist() |> 
      unname())



left_vars_census <- paste0(left_vars, "_q5")
left_vars_census_bivar <- paste0(left_vars, "_q3")

right_vars <-
  variables |> 
  filter(source == "census") |> 
  filter(!theme %in% c("Employment", "Housing"), !is.na(theme)) |> 
  pull(var_code)







for (var in left_vars) {
  
  var_year <-
    variables |> 
    filter(var_code == var) |> 
    pull(dates) |> 
    pluck(1)
  
  var_year_comb <-
    expand.grid(var_year, var_year) |> 
    as_tibble() |> 
    mutate(across(everything(), as.character)) |> 
    mutate(across(everything(), as.numeric)) |> 
    filter(Var1 < Var2) |> 
    as.vector() |> 
    split(1:10) |> 
    map(unlist) |> 
    map(unname)
  
  single_years <- 
    map_dfc(var_year, ~borough[[paste(var, "q5", .x, sep = "_")]]) |> 
    set_names(paste(var, var_year, sep = "_"))
  
  multi_years <- 
    map_dfc(var_year_comb, ~{
      
      
    })
  
  borough |> 
    select(ID, name, contains(all_of(var)), geometry)
  
  
  
}












# Construct tile lookup table ---------------------------------------------

tile_lookup <- qread("data/tile_lookup.qs")

tile_lookup <- 
  tile_lookup |> 
  bind_rows(
    tibble(
      module = "climate_risk",
      tile2 = left_vars,
      suffix = paste0("-", 6:50)  
  )
)

qsave(tile_lookup, "data/tile_lookup.qs")

