#### ALLEY TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
building_full <- qread("data/building_full.qs", nthreads = 4)
variables <- qread("data/variables.qs")
source("dev/tiles/_tile_functions.R")
source("R/functions/_utils.R")
qload("data/colours.qsm")
qload("data/alleys.qsm")

# Get variables to add ----------------------------------------------------

left_vars <- c("green_alley_sqkm", "green_alley_per1k")

all_years <- "2016"

right_vars <-
  variables |> 
  filter(source == "census") |> 
  filter(!theme %in% c("Employment"), !is.na(theme)) |> 
  pull(var_code)


# Individual alley tile ---------------------------------------------------

alleys |> 
  select(ID, type) |> 
  mutate(ID = as.character(ID)) |> 
  mutate(type = case_when(type == "green" ~ "20",
                          type == "community" ~ "21",
                          type == "mixed" ~ "22",
                          type == "none" ~ "23")) |>
  upload_tile_source("alley-individual2", username = "maxbdb2")

alley_individual <- 
  create_recipe(
    layer_names = "alley-individual",
    source = "mapbox://tileset-source/maxbdb2/alley-individual2",
    minzoom = 3,
    maxzoom = 16, 
    layer_size = 2500,
    recipe_name = "alley-individual")

create_tileset("alley-individual", alley_individual, username = "maxbdb2")
publish_tileset("alley-individual", username = "maxbdb2")


# Empty borough -----------------------------------------------------------

borough |> 
  select(ID) |> 
  filter(ID %in% alley_text$ID) |> 
  st_set_agr("constant") |> 
  upload_tile_source("alley-empty_borough", "maxbdb2", .sus_token)

alley_empty_borough <- 
  create_recipe(
    layer_names = "alley-empty_borough",
    source = "mapbox://tileset-source/maxbdb2/alley-empty_borough",
    minzoom = 3,
    maxzoom = 11, 
    layer_size = 2500,
    recipe_name = "alley-empty_borough")

create_tileset("alley-empty_borough", alley_empty_borough, "maxbdb2")
publish_tileset("alley-empty_borough", "maxbdb2")


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

borough_1 <- 
  borough |> 
  as_tibble() |> 
  filter(CSDUID %in% alley_text$ID) |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"), 
                ~paste(green_alley_sqkm_q3, .x, sep = " - "))) |> 
  relocate(green_alley_sqkm_q5, .after = name) |> 
  select(-green_alley_sqkm_q3) |> 
  rename_with(~paste0("green_alley_sqkm_", str_remove(.x, "_q3")),
              contains("_q3")) |> 
  rename(green_alley_sqkm = green_alley_sqkm_q5) |> 
  mutate(green_alley_sqkm = as.character(green_alley_sqkm),
         across(c(-ID, -name, -green_alley_sqkm, -geometry), 
                trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") 

borough_2 <- 
  borough |> 
  as_tibble() |> 
  filter(CSDUID %in% alley_text$ID) |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"),
                ~paste(green_alley_per1k_q3, .x, sep = " - "))) |>
  relocate(green_alley_per1k_q5, .after = name) |>
  select(-green_alley_per1k_q3) |>
  rename_with(~paste0("green_alley_per1k_", str_remove(.x, "_q3")),
              contains("_q3")) |>
  rename(green_alley_per1k = green_alley_per1k_q5) |>
  mutate(green_alley_per1k = as.character(green_alley_per1k),
         across(c(-ID, -name, -green_alley_per1k, -geometry), 
                trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") 

left_join(borough_1, st_drop_geometry(borough_2), by = c("ID", "name")) |> 
  upload_tile_source("alley-borough", "maxbdb2", .sus_token)


# Process CT then upload tile source ---------------------------------

CT_1 <- 
  CT |> 
  as_tibble() |> 
  filter(CSDUID %in% alley_text$ID) |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"), 
                ~paste(green_alley_sqkm_q3, .x, sep = " - "))) |> 
  relocate(green_alley_sqkm_q5, .after = name) |> 
  select(-green_alley_sqkm_q3) |> 
  rename_with(~paste0("green_alley_sqkm_", str_remove(.x, "_q3")),
              contains("_q3")) |> 
  rename(green_alley_sqkm = green_alley_sqkm_q5) |> 
  mutate(green_alley_sqkm = as.character(green_alley_sqkm),
         across(c(-ID, -name, -green_alley_sqkm, -geometry), 
                trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") 

CT_2 <- 
  CT |> 
  as_tibble() |> 
  filter(CSDUID %in% alley_text$ID) |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"),
                ~paste(green_alley_per1k_q3, .x, sep = " - "))) |>
  relocate(green_alley_per1k_q5, .after = name) |>
  select(-green_alley_per1k_q3) |>
  rename_with(~paste0("green_alley_per1k_", str_remove(.x, "_q3")),
              contains("_q3")) |>
  rename(green_alley_per1k = green_alley_per1k_q5) |>
  mutate(green_alley_per1k = as.character(green_alley_per1k),
         across(c(-ID, -name, -green_alley_per1k, -geometry), 
                trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") 

left_join(CT_1, st_drop_geometry(CT_2), by = c("ID", "name")) |> 
  upload_tile_source("alley-CT", "maxbdb2", .sus_token)


# Process DA then upload tile source ---------------------------------

DA_1 <- 
  DA |> 
  as_tibble() |> 
  filter(CSDUID %in% alley_text$ID) |>
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"), 
                ~paste(green_alley_sqkm_q3, .x, sep = " - "))) |> 
  relocate(green_alley_sqkm_q5, .after = name) |> 
  select(-green_alley_sqkm_q3) |> 
  rename_with(~paste0("green_alley_sqkm_", str_remove(.x, "_q3")),
              contains("_q3")) |> 
  rename(green_alley_sqkm = green_alley_sqkm_q5) |> 
  mutate(green_alley_sqkm = as.character(green_alley_sqkm),
         across(c(-ID, -name, -green_alley_sqkm, -geometry), 
                trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") 

DA_2 <- 
  DA |> 
  as_tibble() |> 
  filter(CSDUID %in% alley_text$ID) |>
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"),
                ~paste(green_alley_per1k_q3, .x, sep = " - "))) |>
  relocate(green_alley_per1k_q5, .after = name) |>
  select(-green_alley_per1k_q3) |>
  rename_with(~paste0("green_alley_per1k_", str_remove(.x, "_q3")),
              contains("_q3")) |>
  rename(green_alley_per1k = green_alley_per1k_q5) |>
  mutate(green_alley_per1k = as.character(green_alley_per1k),
         across(c(-ID, -name, -green_alley_per1k, -geometry), 
                trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") 

left_join(DA_1, st_drop_geometry(DA_2), by = c("ID", "name")) |> 
  upload_tile_source("alley-DA", "maxbdb2", .sus_token)


# Process building then upload tile source --------------------------------

# building_to_process <- 
#   building_full |> 
#   as_tibble() |> 
#   select(ID, name, all_of(vars_to_add), geometry) |> 
#   mutate(across(contains("_q3"), 
#                 ~paste(canale_ind_q3_2016, .x, sep = " - "))) |> 
#   relocate(canale_ind_q5_2016, .after = name) |> 
#   select(-canale_ind_q3_2016) |> 
#   rename_with(~paste0("canale_ind_2016_", str_remove(.x, "_q3")),
#               contains("_q3")) |> 
#   rename(canale_ind_2016 = canale_ind_q5_2016) |> 
#   mutate(canale_ind_2016 = as.character(canale_ind_2016),
#          across(c(-ID, -name, -canale_ind_2016, -geometry), trans_var)) |> 
#   st_as_sf() |> 
#   st_set_agr("constant")
# 
# iter_size <- ceiling(nrow(building_to_process) / 100)
# 
# building_to_process_list <- 
#   map(1:100, ~{
#     building_to_process |> 
#       slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
#       geojsonsf::sf_geojson() |> 
#       paste0(collapse = " ") |> 
#       geojson::featurecollection()  
#   })
# 
# # Iteratively post files to tile source
# tmp <- tempfile(fileext = ".json")
# tmp_list <- map(1:10, ~tempfile(fileext = ".json"))
# 
# map(1:10, ~{
#   
#   print(.x)
#   to_process <- building_to_process_list[((.x - 1) * 10 + 1):(.x * 10)]
#   walk2(to_process, tmp_list, geojson::ndgeo_write)
#   
#   # Concatenate geoJSONs
#   out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
#   system(out)
#   
#   # Upload to MTS
#   out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
#                 'sus-mcgill/canale-building?access_token=', .sus_token, 
#                 '" -F file=@', tmp, 
#                 ' --header "Content-Type: multipart/form-data"')
#   system(out)
#   
# })


# Process DA_building then upload tile source -----------------------------

# DA |> 
#   st_set_geometry("building") |> 
#   as_tibble() |> 
#   select(ID, name, all_of(vars_to_add), geometry = building) |> 
#   mutate(across(contains("_q3"), 
#                 ~paste(canale_ind_q3_2016, .x, sep = " - "))) |> 
#   relocate(canale_ind_q5_2016, .after = name) |> 
#   select(-canale_ind_q3_2016) |> 
#   rename_with(~paste0("canale_ind_2016_", str_remove(.x, "_q3")),
#               contains("_q3")) |> 
#   rename(canale_ind_2016 = canale_ind_q5_2016) |> 
#   mutate(canale_ind_2016 = as.character(canale_ind_2016),
#          across(c(-ID, -name, -canale_ind_2016, -geometry), trans_var)) |> 
#   st_as_sf() |> 
#   st_set_agr("constant") |>
#   filter(!st_is_empty(geometry)) |> 
#   upload_tile_source("canale-DA_building", "sus-mcgill", .sus_token)


# Add recipes -------------------------------------------------------------

recipe_borough <- 
  create_recipe(
    layer_names = "borough",
    source = "mapbox://tileset-source/maxbdb2/alley-borough",
    minzoom = 3,
    maxzoom = 11, 
    simplification_zoom = 11,
    recipe_name = "alley-borough")

recipe_CT <- 
  create_recipe(
    layer_names = "CT",
    source = "mapbox://tileset-source/maxbdb2/alley-CT",
    minzoom = 3,
    maxzoom = 12, 
    simplification_zoom = 12,
    recipe_name = "alley-CT")

recipe_DA <- 
  create_recipe(
    layer_names = c("DA_empty", "DA"),
    source = c(
      DA_empty = "mapbox://tileset-source/maxbdb2/DA_empty",
      DA = "mapbox://tileset-source/maxbdb2/alley-DA"),
    minzoom = c(DA_empty = 3, DA = 9),
    maxzoom = c(DA_empty = 8, DA = 13), 
    layer_size = c(DA_empty = NA, DA = 2500),
    simplification_zoom = c(DA_empty = NA, DA = 13),
    recipe_name = "alley-DA")


recipe_building <- 
  create_recipe(
    layer_names = c("DA_building_empty", "DA_building", "building"),
    source = c(
      DA_building_empty = "mapbox://tileset-source/maxbdb2/DA_building_empty",
      DA_building = "mapbox://tileset-source/maxbdb2/alley-DA_building",
      building = "mapbox://tileset-source/maxbdb2/alley-building"),
    minzoom = c(DA_building_empty = 3, DA_building = 9, building = 14),
    maxzoom = c(DA_building_empty = 8, DA_building = 13, building = 16), 
    layer_size = c(DA_building_empty = NA, DA_building = NA, building = 2500),
    recipe_name = "alley-building")



recipe_auto_zoom <- 
  create_recipe(
    layer_names = c("borough", "CT", "DA", "building"),
    source = c(
      borough = "mapbox://tileset-source/maxbdb2/alley-borough",
      CT = "mapbox://tileset-source/maxbdb2/alley-CT",
      DA = "mapbox://tileset-source/maxbdb2/alley-DA",
      building = "mapbox://tileset-source/maxbdb2/alley-building"),
    minzoom = c(borough = 2, CT = 11, DA = 13, building = 16),
    maxzoom = c(borough = 10, CT = 12, DA = 15, building = 16), 
    layer_size = c(borough = NA, CT = NA, DA = NA, building = 2500),
    recipe_name = "alley-auto_zoom")


# Create and publish tilesets ---------------------------------------------

create_tileset("alley-borough", recipe_borough, username = "maxbdb2")
publish_tileset("alley-borough", username = "maxbdb2")

create_tileset("alley-CT", recipe_CT, username = "maxbdb2")
publish_tileset("alley-CT", username = "maxbdb2")

create_tileset("alley-DA", recipe_DA, username = "maxbdb2")
publish_tileset("alley-DA", username = "maxbdb2")

create_tileset("alley-building", recipe_building)
publish_tileset("alley-building")

create_tileset("alley-auto_zoom", recipe_auto_zoom)
publish_tileset("alley-auto_zoom")