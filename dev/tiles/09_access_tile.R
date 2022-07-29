#### ACCESS TILE PROCESSING ###################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
metro_lines <- qread("data/metro_lines.qs")
variables <- qread("data/variables.qs")
source("dev/tiles/_tile_functions.R")
source("R/functions/_utils.R")
qload("data/colours.qsm")


# Upload metro lines ------------------------------------------------------

metro_lines |> 
  upload_tile_source("metro_lines")

metro_lines_recipe <- 
  create_recipe(
    layer_names = "metro_lines",
    source = "mapbox://tileset-source/sus-mcgill/metro_lines",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    recipe_name = "metro_lines")

create_tileset("metro_lines", metro_lines_recipe)
publish_tileset("metro_lines")


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

vars_to_add <- 
  variables |> 
  filter(source == "census") |> 
  filter(theme != "Employment", !is.na(theme)) |> 
  pull(var_code) |> 
  paste0("_q3_2016") |> 
  c(paste0(left_vars, "_q3")) |>
  c(paste0(left_vars, "_q5"))


# Process CT then upload tile source --------------------------------------

CT_data <-
  map(left_vars, function(left_var) {
    
    vars_to_add_one_access <- 
      c(vars_to_add[!str_starts(vars_to_add, "access")], 
        paste0(left_var, "_q3"), paste0(left_var, "_q5"))
    
    out <- 
      CT |> 
      as_tibble() |> 
      select(ID, name, all_of(vars_to_add_one_access), geometry) |> 
      mutate(across(contains("_q3"), 
                    ~paste(eval(parse(text = paste0(left_var, "_q3"))), .x, 
                           sep = " - "))) |> 
      relocate(all_of(paste0(left_var, "_q5")), .after = name) |> 
      select(-all_of(paste0(left_var, "_q3"))) |>
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



# Split data for upload ---------------------------------------------------

CT_data_list <- 
  map(left_vars, ~select(CT_data, ID, name, starts_with(.x), geometry))


# Construct tile lookup table ---------------------------------------------

tile_lookup <- 
  qread("data/tile_lookup.qs") |> 
  filter(module != "access") |> 
  bind_rows(
    tibble(module = "access", 
           tile2 = left_vars,
           suffix = paste0("-", seq_along(left_vars)))
  )

qsave(tile_lookup, "data/tile_lookup.qs")
  

# CT upload source --------------------------------------------------------

CT_data_list |> 
  walk2(seq_along(left_vars), ~upload_tile_source(.x, paste0("access-CT-", .y)))


# Add recipes -------------------------------------------------------------

recipe_CT <- 
  map(seq_along(left_vars), ~create_recipe(
    layer_names = "CT",
    source = paste0("mapbox://tileset-source/sus-mcgill/access-CT-", .x),
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    layer_size = 2500,
    recipe_name = paste0("access-CT-", .x)))


# Create and publish tilesets ---------------------------------------------

# Create tilesets
for (i in seq_along(left_vars)) {
  out <- create_tileset(paste0("access-CT-", i), recipe_CT[[i]])
  if (out$status_code != 200) stop(var)
  Sys.sleep(1)
}

# Publish tilesets
for (i in seq_along(left_vars)) {
  out <- publish_tileset(paste0("access-CT-", i))
  if (out$status_code != 200) stop(var)
  Sys.sleep(30)
}
