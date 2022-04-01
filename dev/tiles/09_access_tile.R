#### ACCESS TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
building_full <- qread("data/building_full.qs", nthreads = 4)
variables <- qread("data/variables.qs")
source("dev/tiles/_tile_functions.R")
source("R/functions/_utils.R")
qload("data/colours.qsm")
qload("data/accesss.qsm")
qload("data/colours.qsm")

# Get variables to add ----------------------------------------------------

left_vars <- 
  variables |> 
  filter(str_starts(var_code, "access")) |> 
  pull("var_code")

right_vars <-
  variables |> 
  filter(source == "census") |> 
  filter(!theme %in% c("Employment"), !is.na(theme)) |> 
  pull(var_code)

# Empty CT ---------------------------------------------------------------

CT |> 
  select(ID) |> 
  st_set_agr("constant") |> 
  upload_tile_source("access-empty_CT", .sus_token)

access_empty_CT <- 
  create_recipe(
    layer_names = "access-empty_CT",
    source = "mapbox://tileset-source/sus-mcgill/access-empty_CT",
    minzoom = 3,
    maxzoom = 12, 
    simplification_zoom = 12,
    recipe_name = "access-empty_CT")

create_tileset("access-empty_CT", access_empty_CT)
publish_tileset("access-empty_CT")


### Processing function ---------------------------------------------------

# Get variables to add ----------------------------------------------------

vars_to_add <- 
  variables |> 
  filter(source == "census") |> 
  filter(theme != "Employment", !is.na(theme)) |> 
  pull(var_code) |> 
  paste0("_q3_2016") |> 
  c(paste0(left_vars, "_q3")) |>
  c(paste0(left_vars, "_q5"))


# Process borough then upload tile source ---------------------------------

CT_data <-
map(left_vars, function(left_var) {
  
  vars_to_add_one_access <- 
    c(vars_to_add[!str_starts(vars_to_add, "access")], paste0(left_var, "_q3"),
      paste0(left_var, "_q5"))
  
  out <- 
    CT |> 
    as_tibble() |> 
    select(ID, name, all_of(vars_to_add_one_access), geometry) |> 
    mutate(across(contains("_q3"), 
                  ~paste(eval(parse(text = paste0(left_var, "_q3"))), .x, sep = " - "))) |> 
    relocate(paste0(left_var, "_q5"), .after = name) |> 
    select(-paste0(left_var, "_q3")) |>
    rename_with(~paste0(paste0(left_var, "_"), str_remove(.x, "_q3")),
                contains("_q3")) |> 
    rename_with(~paste0(left_var), paste0(left_var, "_q5"))
  
  out[[left_var]] <- as.character(out[[left_var]])
  
  out <- 
    out |> 
    mutate(across(c(-ID, -name, -left_var, -geometry), trans_var)) |> 
    st_as_sf() |> 
    st_set_agr("constant") 
  
  if (left_var != left_vars[1]) out <- st_drop_geometry(out)
  
  out
  
}) |> reduce(left_join, by = c("ID", "name"))


# Test the data -----------------------------------------------------------

right_vars <-   
  variables |> 
  filter(source == "census") |> 
  filter(theme != "Employment", !is.na(theme)) |> 
  pull(var_code) |> 
  paste0("_2016") |> 
  c(" ")

test_data_to_upload("CT", CT_data, left_vars, right_vars)
                        

# CT upload source --------------------------------------------------------

CT_data |> 
  upload_tile_source("access-CT")

# Add recipes -------------------------------------------------------------

recipe_CT <- 
  create_recipe(
    layer_names = "CT",
    source = "mapbox://tileset-source/sus-mcgill/access-CT",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = "access-CT")

# Create and publish tilesets ---------------------------------------------

create_tileset("access-CT", recipe_CT)
publish_tileset("access-CT")
