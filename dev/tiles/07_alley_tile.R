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
qload("data/alley.qsm")


# Get variables to add ----------------------------------------------------

left_vars <- c("green_alley_sqkm", "green_alley_per1k")

right_vars <-
  variables |> 
  filter(source == "census") |> 
  filter(!theme %in% c("Employment"), !is.na(theme)) |> 
  pull(var_code)


# Individual alley tile ---------------------------------------------------

alley_alley <- 
  alley |> 
  select(ID, type) |> 
  mutate(ID = as.character(ID)) |> 
  mutate(type = case_when(type == "green" ~ "20",
                          type == "community" ~ "21",
                          type == "mixed" ~ "22",
                          type == "none" ~ "23"))

ggplot(alley_alley) +
  geom_sf(aes(fill = type), colour = "transparent") +
  scale_fill_manual(values = setNames(colour_table$value, colour_table$group)) +
  theme_void()

upload_tile_source(alley_alley, "alley-alley")

alley_alley_recipe <- 
  create_recipe(
    layer_names = "alley-alley",
    source = "mapbox://tileset-source/sus-mcgill/alley-alley",
    minzoom = 3,
    maxzoom = 16, 
    layer_size = 2500,
    simp_zoom = 1,
    fallback_simp_zoom = 1,
    recipe_name = "alley-alley")

create_tileset("alley-alley", alley_alley_recipe)
publish_tileset("alley-alley")


# Empty borough -----------------------------------------------------------

borough |> 
  select(ID) |> 
  filter(ID %in% alley_text$ID) |> 
  st_set_agr("constant") |> 
  upload_tile_source("alley-borough_empty")

alley_borough_empty_recipe <- 
  create_recipe(
    layer_names = "alley-borough_empty",
    source = "mapbox://tileset-source/sus-mcgill/alley-borough_empty",
    minzoom = 3,
    maxzoom = 11, 
    simp_zoom = 11,
    layer_size = 2500,
    recipe_name = "alley-borough_empty")

create_tileset("alley-borough_empty", alley_borough_empty_recipe)
publish_tileset("alley-borough_empty")


# Get variables to add ----------------------------------------------------

vars_to_add <- 
  variables |> 
  filter(source == "census") |> 
  filter(theme != "Employment", !is.na(theme)) |> 
  pull(var_code) |> 
  paste0("_q3_2016") |> 
  c(paste0(left_vars, "_q3")) |>
  c(paste0(left_vars, "_q5"))


# Processing function -----------------------------------------------------

make_alley_tiles <- function(df) {
  
  df_1 <- 
    get(df) |> 
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
  
  df_2 <- 
    get(df) |> 
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
  
  left_join(df_1, st_drop_geometry(df_2), by = c("ID", "name"))
}


# Process census geographies and upload tileset sources -------------------

make_alley_tiles("borough") |> upload_tile_source("alley-borough")
make_alley_tiles("CT") |> upload_tile_source("alley-CT")
make_alley_tiles("DA") |> upload_tile_source("alley-DA")



# Process building --------------------------------------------------------

DA |> 
  filter(CSDUID %in% alley_text$ID) |> 
  st_set_geometry("building") |> 
  select(ID, name, geometry = building) |> 
  left_join(select(st_drop_geometry(make_alley_tiles("DA")), -name), 
            by = "ID") |> 
  relocate(geometry, .after = last_col()) |>
  filter(!st_is_empty(geometry)) |> 
  upload_tile_source("alley-DA_building")
  
building_to_process <-
  building_full |> 
  filter(CSDUID %in% alley_text$ID) |> 
  select(ID:DAUID) |> 
  left_join(select(st_drop_geometry(make_alley_tiles("DA")), -name),
            by = c("DAUID" = "ID"))
  
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

for (j in 1:10) {
  
  try <- 0L
  success <- FALSE
  
  while (try < 4 && !success) {
    
    try <- try + 1L
    
    cat(paste0("\n", j, "\n"))
    to_process <- building_to_process_list[((j - 1) * 10 + 1):(j * 10)]
    walk2(to_process, tmp_list, geojson::ndgeo_write)
    
    # Concatenate geoJSONs
    out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
    system(out)
    
    # Upload to MTS
    out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                  'sus-mcgill/alley-building?access_token=', 
                  .sus_token, '" -F file=@', tmp, 
                  ' --header "Content-Type: multipart/form-data"')
    system(out)
    
    file_n <- 
      list_tile_sources() |> 
      filter(str_detect(id, "alley-building")) |> 
      pull(files)
    
    if (file_n == j) success <- TRUE
    
  }
  
  if (!success) stop("UPLOAD ERROR")
}


# Add recipes -------------------------------------------------------------

recipe_borough <- 
  create_recipe(
    layer_names = "borough",
    source = "mapbox://tileset-source/sus-mcgill/alley-borough",
    minzoom = 3,
    maxzoom = 11, 
    simp_zoom = 11,
    recipe_name = "alley-borough")

recipe_CT <- 
  create_recipe(
    layer_names = "CT",
    source = "mapbox://tileset-source/sus-mcgill/alley-CT",
    minzoom = 3,
    maxzoom = 12, 
    simp_zoom = 12,
    recipe_name = "alley-CT")

recipe_DA <- 
  create_recipe(
    layer_names = c("DA_empty", "DA"),
    source = c(
      DA_empty = "mapbox://tileset-source/sus-mcgill/DA_empty",
      DA = "mapbox://tileset-source/sus-mcgill/alley-DA"),
    minzoom = c(DA_empty = 3, DA = 9),
    maxzoom = c(DA_empty = 8, DA = 13), 
    layer_size = c(DA_empty = NA, DA = 2500),
    simp_zoom = c(DA_empty = NA, DA = 13),
    recipe_name = "alley-DA")

recipe_building <- 
  create_recipe(
    layer_names = c("DA_building_empty", "DA_building", "building"),
    source = c(
      DA_building_empty = "mapbox://tileset-source/sus-mcgill/DA_building_empty",
      DA_building = "mapbox://tileset-source/sus-mcgill/alley-DA_building",
      building = "mapbox://tileset-source/sus-mcgill/alley-building"),
    minzoom = c(DA_building_empty = 3, DA_building = 9, building = 14),
    maxzoom = c(DA_building_empty = 8, DA_building = 13, building = 16), 
    layer_size = c(DA_building_empty = NA, DA_building = NA, building = 2500),
    recipe_name = "alley-building")

recipe_auto_zoom <- 
  create_recipe(
    layer_names = c("borough", "CT", "DA", "building"),
    source = c(
      borough = "mapbox://tileset-source/sus-mcgill/alley-borough",
      CT = "mapbox://tileset-source/sus-mcgill/alley-CT",
      DA = "mapbox://tileset-source/sus-mcgill/alley-DA",
      building = "mapbox://tileset-source/sus-mcgill/alley-building"),
    minzoom = c(borough = 2, CT = 11, DA = 13, building = 16),
    maxzoom = c(borough = 10, CT = 12, DA = 15, building = 16), 
    layer_size = c(borough = NA, CT = NA, DA = NA, building = 2500),
    recipe_name = "alley-auto_zoom")


# Create and publish tilesets ---------------------------------------------

create_tileset("alley-borough", recipe_borough)
publish_tileset("alley-borough")

create_tileset("alley-CT", recipe_CT)
publish_tileset("alley-CT")

create_tileset("alley-DA", recipe_DA)
publish_tileset("alley-DA")

create_tileset("alley-building", recipe_building)
publish_tileset("alley-building")

create_tileset("alley-auto_zoom", recipe_auto_zoom)
publish_tileset("alley-auto_zoom")
