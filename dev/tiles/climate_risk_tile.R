#### CLIMATE_RISK TILE PROCESSING ##############################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
grid <- qread("data/grid.qs")
building_full <- qread("data/building_full.qs")
variables <- qread("data/variables.qs")
source("dev/tiles/tile_functions.R")


# Get variables to add ----------------------------------------------------

left_vars <-
  variables |> 
  filter(theme == "Climate") |> 
  pull(var_code)

left_vars_census <- paste0(left_vars, "_q5")
left_vars_census_bivar <- paste0(left_vars, "_q3")

right_vars <-
  variables |> 
  filter(source == "census") |> 
  filter(theme != "Employment", !is.na(theme)) |> 
  pull(var_code) |> 
  paste0("_q3_2016")


# Construct tile lookup table ---------------------------------------------

tile_lookup <- tibble(
  module = "climate_risk",
  tile2 = left_vars,
  suffix = paste0("-", 1:5)
)

qsave(tile_lookup, "data/tile_lookup.qs")


# Process grid ------------------------------------------------------------

# Upload tile source
map(left_vars, ~{
  grid |> 
    as_tibble() |> 
    select(ID, name, all_of(.x), geometry) |> 
    mutate(across(all_of(.x), as.character)) |> 
    mutate(across(all_of(left_vars[i]), replace_na, "0")) |> 
    bind_cols({
      map_dfc(right_vars, \(y) 
              paste(grid[[paste0(.x, "_q3")]], grid[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(.x, str_replace(right_vars, "_q3", ""), sep = "_"))
    }) |> 
    relocate(geometry, .after = last_col()) |> 
    st_as_sf() |> 
    st_set_agr("constant") |> 
    upload_tile_source(paste0("climate_risk-grid", tile_lookup$suffix[
      tile_lookup$module == "climate_risk" & tile_lookup$tile2 == .x]))
})

# Create recipes
grid_recipes <- 
  map(left_vars, ~{
    suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                   tile_lookup$tile2 == .x]
    create_recipe(
      layer_names = "grid",
      source = paste0(
        "mapbox://tileset-source/sus-mcgill/climate_risk-grid", suffix),
      minzoom = 3,
      maxzoom = 13, 
      layer_size = 2500,
      simplification_zoom = 13,
      recipe_name = paste0("climate_risk-grid", suffix))
  })

# Create tilesets
for (i in seq_along(left_vars)) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == left_vars[i]]
  create_tileset(paste0("climate_risk-grid", suffix), grid_recipes[[i]])
  Sys.sleep(2)
}

# Publish tilesets
for (var in left_vars) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == var]
  publish_tileset(paste0("climate_risk-grid", suffix))
  Sys.sleep(2)
}


# Process borough ---------------------------------------------------------

# Upload tile source
map(left_vars, ~{
  borough |> 
    as_tibble() |> 
    select(ID, name, all_of(.x), geometry) |> 
    mutate(across(all_of(.x), as.character)) |> 
    mutate(across(all_of(left_vars[i]), replace_na, "0")) |> 
    bind_cols({
      map_dfc(right_vars, \(y) 
              paste(borough[[paste0(.x, "_q3")]], borough[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(.x, str_replace(right_vars, "_q3", ""), sep = "_"))
    }) |> 
    relocate(geometry, .after = last_col()) |> 
    st_as_sf() |> 
    st_set_agr("constant") |> 
    upload_tile_source(paste0("climate_risk-borough", tile_lookup$suffix[
      tile_lookup$module == "climate_risk" & tile_lookup$tile2 == .x]))
})

# Create recipes
borough_recipes <- 
  map(left_vars, ~{
    suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                   tile_lookup$tile2 == .x]
    create_recipe(
      layer_names = "borough",
      source = paste0(
        "mapbox://tileset-source/sus-mcgill/climate_risk-borough", suffix),
      minzoom = 3,
      maxzoom = 11, 
      layer_size = 2500,
      simplification_zoom = 11,
      recipe_name = paste0("climate_risk-borough", suffix))
  })

# Create tilesets
for (i in seq_along(left_vars)) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == left_vars[i]]
  create_tileset(paste0("climate_risk-borough", suffix), 
                 borough_recipes[[i]])
  Sys.sleep(2)
}

# Publish tilesets
for (var in left_vars) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == var]
  publish_tileset(paste0("climate_risk-borough", suffix))
  Sys.sleep(2)
}


# Process CT --------------------------------------------------------------

# Upload tile source
map(left_vars, ~{
  CT |> 
    as_tibble() |> 
    select(ID, name, all_of(.x), geometry) |> 
    mutate(across(all_of(.x), as.character)) |> 
    mutate(across(all_of(left_vars[i]), replace_na, "0")) |> 
    bind_cols({
      map_dfc(right_vars, \(y) 
              paste(CT[[paste0(.x, "_q3")]], CT[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(.x, str_replace(right_vars, "_q3", ""), sep = "_"))
    }) |> 
    relocate(geometry, .after = last_col()) |> 
    st_as_sf() |> 
    st_set_agr("constant") |> 
    upload_tile_source(paste0("climate_risk-CT", tile_lookup$suffix[
      tile_lookup$module == "climate_risk" & tile_lookup$tile2 == .x]))
})

# Create recipes
CT_recipes <- 
  map(left_vars, ~{
    suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                   tile_lookup$tile2 == .x]
    create_recipe(
      layer_names = "CT",
      source = paste0(
        "mapbox://tileset-source/sus-mcgill/climate_risk-CT", suffix),
      minzoom = 3,
      maxzoom = 12, 
      layer_size = 2500,
      simplification_zoom = 12,
      recipe_name = paste0("climate_risk-CT", suffix))
  })

# Create tilesets
for (i in seq_along(left_vars)) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == left_vars[i]]
  create_tileset(paste0("climate_risk-CT", suffix), 
                 CT_recipes[[i]])
  Sys.sleep(2)
}

# Publish tilesets
for (var in left_vars) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == var]
  publish_tileset(paste0("climate_risk-CT", suffix))
  Sys.sleep(2)
}


# Process DA --------------------------------------------------------------

# Upload tile source
map(left_vars, ~{
  DA |> 
    as_tibble() |> 
    select(ID, name, all_of(.x), geometry) |> 
    mutate(across(all_of(.x), as.character)) |> 
    mutate(across(all_of(left_vars[i]), replace_na, "0")) |> 
    bind_cols({
      map_dfc(right_vars, \(y) 
              paste(DA[[paste0(.x, "_q3")]], DA[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(.x, str_replace(right_vars, "_q3", ""), sep = "_"))
    }) |> 
    relocate(geometry, .after = last_col()) |> 
    st_as_sf() |> 
    st_set_agr("constant") |> 
    upload_tile_source(paste0("climate_risk-DA", tile_lookup$suffix[
      tile_lookup$module == "climate_risk" & tile_lookup$tile2 == .x]))
})

# Create recipes
DA_recipes <- 
  map(left_vars, ~{
    
    suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                   tile_lookup$tile2 == .x]
    
    create_recipe(
      layer_names = c("DA_empty", "DA"),
      source = c(
        DA_empty = "mapbox://tileset-source/sus-mcgill/DA_empty",
        DA = paste0("mapbox://tileset-source/sus-mcgill/climate_risk-DA", 
                    suffix)),
      minzoom = c(DA_empty = 3, DA = 9),
      maxzoom = c(DA_empty = 8, DA = 13), 
      layer_size = c(DA_empty = NA, DA = 2500),
      simplification_zoom = c(DA_empty = NA, DA = 13),
      recipe_name = paste0("climate_risk-DA", suffix))
  })

# Create tilesets
for (i in seq_along(left_vars)) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == left_vars[i]]
  create_tileset(paste0("climate_risk-DA", suffix), 
                 DA_recipes[[i]])
  Sys.sleep(2)
}

# Publish tilesets
for (var in left_vars) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == var]
  out <- publish_tileset(paste0("climate_risk-DA", suffix))
  if (!str_detect(httr::content(out)$message, "Processing")) break
  Sys.sleep(2)
}


# Process building --------------------------------------------------------

# Upload building tile source
building_to_process <- vector("list", length(left_vars))

for (i in seq_along(left_vars)) {
  building_to_process[[i]] <- 
    building_full |> 
    as_tibble() |> 
    select(ID, name, all_of(left_vars[i]), geometry) |> 
    mutate(across(all_of(left_vars[i]), as.character)) |> 
    mutate(across(all_of(left_vars[i]), replace_na, "0")) |> 
    bind_cols({
      map_dfc(right_vars, \(y) trans_var(paste(
        building_full[[paste0(left_vars[i], "_q3")]], building_full[[y]], 
        sep = " - "))) |> 
        set_names(paste(left_vars[i], str_replace(right_vars, "_q3", ""), 
                        sep = "_"))}) |> 
    relocate(geometry, .after = last_col()) |> 
    st_as_sf() |> 
    st_set_agr("constant")
}

for (i in seq_along(left_vars)) {
  
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == left_vars[i]]
  
  iter_size <- ceiling(nrow(building_to_process[[1]]) / 100)
  
  building_to_process_list <- 
    map(1:100, ~{
      building_to_process[[i]] |> 
        slice(((.x - 1) * iter_size + 1):(.x * iter_size)) |> 
        geojsonsf::sf_geojson() |> 
        paste0(collapse = " ") |> 
        geojson::featurecollection()  
    })
  
  # Iteratively post files to tile source
  tmp <- tempfile(fileext = ".json")
  tmp_list <- map(1:10, ~tempfile(fileext = ".json"))
  
  for (j in 1:10) {
    
    print(j)
    to_process <- building_to_process_list[((j - 1) * 10 + 1):(j * 10)]
    walk2(to_process, tmp_list, geojson::ndgeo_write)
    
    # Concatenate geoJSONs
    out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
    system(out)
    
    # Upload to MTS
    out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                  'sus-mcgill/climate_risk-building', suffix,
                  '?access_token=', .sus_token, '" -F file=@', tmp, 
                  ' --header "Content-Type: multipart/form-data"')
    system(out)
    
  }
  
}

# Upload DA_building tile source
map(left_vars, ~{
  
  DA |> 
    st_set_geometry("building") |> 
    as_tibble() |> 
    select(ID, name, all_of(.x), geometry = building) |> 
    mutate(across(all_of(.x), as.character)) |> 
    mutate(across(all_of(.x), replace_na, "0")) |> 
    bind_cols({
      map_dfc(right_vars, \(y) 
              paste(DA[[paste0(.x, "_q3")]], DA[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(.x, str_replace(right_vars, "_q3", ""), sep = "_"))
    }) |> 
    relocate(geometry, .after = last_col()) |> 
    st_as_sf() |> 
    st_set_agr("constant") |> 
    filter(!st_is_empty(geometry)) |> 
    upload_tile_source(paste0("climate_risk-DA_building", tile_lookup$suffix[
      tile_lookup$module == "climate_risk" & tile_lookup$tile2 == .x]))
})

# Create recipes
building_recipes <- 
  map(left_vars, ~{
    
    suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                   tile_lookup$tile2 == .x]
    
    create_recipe(
      layer_names = c("DA_building_empty", "DA_building", "building"),
      source = c(
        DA_building_empty = 
          "mapbox://tileset-source/sus-mcgill/DA_building_empty",
        DA_building = paste0(
          "mapbox://tileset-source/sus-mcgill/climate_risk-DA_building", 
          suffix),
        building = paste0(
          "mapbox://tileset-source/sus-mcgill/climate_risk-building", 
          suffix)),
      minzoom = c(DA_building_empty = 3, DA_building = 9, building = 13),
      maxzoom = c(DA_building_empty = 8, DA_building = 12, building = 16), 
      layer_size = c(DA_building_empty = NA, DA_building = NA, building = 2500),
      recipe_name = paste0("climate_risk-building", suffix))
  })

# Create tilesets
for (i in seq_along(left_vars)) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == left_vars[i]]
  create_tileset(paste0("climate_risk-building", suffix), 
                 building_recipes[[i]])
  Sys.sleep(2)
}

# Publish tilesets
for (var in left_vars) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == var]
  out <- publish_tileset(paste0("climate_risk-building", suffix))
  if (!str_detect(httr::content(out)$message, "Processing")) break
  Sys.sleep(2)
}


# Process auto_zoom -------------------------------------------------------

# Create recipes
auto_zoom_recipes <- 
  map(left_vars, ~{
    
    suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                   tile_lookup$tile2 == .x]
    
    create_recipe(
      layer_names = c("borough", "CT", "DA", "building"),
      source = c(
        borough = paste0(
          "mapbox://tileset-source/sus-mcgill/climate_risk-borough",
          suffix),
        CT = paste0(
          "mapbox://tileset-source/sus-mcgill/climate_risk-CT",
          suffix),
        DA = paste0(
          "mapbox://tileset-source/sus-mcgill/climate_risk-DA", 
          suffix),
        building = paste0(
          "mapbox://tileset-source/sus-mcgill/climate_risk-building", 
          suffix)),
    minzoom = c(borough = 2, CT = 11, DA = 13, building = 16),
    maxzoom = c(borough = 10, CT = 12, DA = 15, building = 16), 
    layer_size = c(borough = NA, CT = NA, DA = NA, building = 2500),
    recipe_name = paste0("climate_risk-auto_zoom", suffix))
  })

# Create tilesets
for (i in seq_along(left_vars)) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == left_vars[i]]
  create_tileset(paste0("climate_risk-auto_zoom", suffix), 
                 auto_zoom_recipes[[i]])
  Sys.sleep(2)
}

# Publish tilesets
for (var in left_vars) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == var]
  out <- publish_tileset(paste0("climate_risk-auto_zoom", suffix))
  if (!str_detect(httr::content(out)$message, "Processing")) break
  Sys.sleep(2)
}
