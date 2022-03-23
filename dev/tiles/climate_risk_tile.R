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
for (var in left_vars[5]) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == var]
  publish_tileset(paste0("climate_risk-grid", suffix))
  Sys.sleep(2)
}


# Process borough then upload tile source ---------------------------------

# Upload tile source
map(left_vars, ~{
  borough |> 
    as_tibble() |> 
    select(ID, name, all_of(.x), geometry) |> 
    mutate(across(all_of(.x), as.character)) |> 
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
for (var in left_vars[5]) {
  suffix <- tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                                 tile_lookup$tile2 == var]
  publish_tileset(paste0("climate_risk-borough", suffix))
  Sys.sleep(2)
}




borough |> 
  as_tibble() |> 
  select(ID, name, all_of(left_vars_census), geometry) |> 
  mutate(across(all_of(left_vars_census), as.character)) |> 
  bind_cols({
    map_dfc(left_vars, \(x) {
      map_dfc(right_vars, \(y) 
              paste(borough[[paste0(x, "_q3")]], borough[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(x, str_replace(right_vars, "_q3", ""), sep = "_"))})
    }) |> 
  relocate(geometry, .after = last_col()) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("climate_risk-borough")


# Process CT then upload tile source ---------------------------------

CT |> 
  as_tibble() |> 
  select(ID, name, all_of(left_vars_census), geometry) |> 
  mutate(across(all_of(left_vars_census), as.character)) |> 
  bind_cols({
    map_dfc(left_vars, \(x) {
      map_dfc(right_vars, \(y) 
              paste(CT[[paste0(x, "_q3")]], CT[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(x, str_replace(right_vars, "_q3", ""), sep = "_"))})
    }) |> 
  relocate(geometry, .after = last_col()) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("climate_risk-CT")


# Process DA then upload tile source ---------------------------------

DA |> 
  as_tibble() |> 
  select(ID, name, all_of(left_vars_census), geometry) |> 
  mutate(across(all_of(left_vars_census), as.character)) |> 
  bind_cols({
    map_dfc(left_vars, \(x) {
      map_dfc(right_vars, \(y) 
              paste(DA[[paste0(x, "_q3")]], DA[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(x, str_replace(right_vars, "_q3", ""), sep = "_"))})
  }) |> 
  relocate(geometry, .after = last_col()) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("climate_risk-DA")


# Process building then upload tile source --------------------------------

building_to_process <- 
  building_full |> 
  as_tibble() |> 
  select(ID, name, all_of(left_vars_census), geometry) |> 
  mutate(across(all_of(left_vars_census), as.character)) |> 
  bind_cols({
    map_dfc(left_vars, \(x) {
      map_dfc(right_vars, \(y) 
              paste(building_full[[paste0(x, "_q3")]], 
                    building_full[[y]], sep = " - ")) |> 
        mutate(across(everything(), trans_var)) |> 
        set_names(paste(x, str_replace(right_vars, "_q3", ""), sep = "_"))})
  }) |> 
  relocate(geometry, .after = last_col()) |> 
  st_as_sf() |> 
  st_set_agr("constant")
  
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

map(1:10, ~{
  
  print(.x)
  to_process <- building_to_process_list[((.x - 1) * 10 + 1):(.x * 10)]
  walk2(to_process, tmp_list, geojson::ndgeo_write)
  
  # Concatenate geoJSONs
  out <- paste0("cat ", paste(tmp_list, collapse = " "), " > ", tmp)
  system(out)
  
  # Upload to MTS
  out <- paste0('curl -X POST "https://api.mapbox.com/tilesets/v1/sources/', 
                'sus-mcgill/climate_risk-building?access_token=', .sus_token, 
                '" -F file=@', tmp, 
                ' --header "Content-Type: multipart/form-data"')
  system(out)
  
})


# Process DA_building then upload tile source -----------------------------

DA |> 
  st_set_geometry("building") |> 
  as_tibble() |> 
  select(ID, name, all_of(left_vars_census), geometry = building) |> 
  mutate(across(all_of(left_vars_census), ~paste0("q5_", .x))) |> 
  bind_cols({
    map_dfc(left_vars, \(x) {
      map_dfc(right_vars, \(y) 
              paste(DA[[paste0(x, "_q3")]], DA[[y]], sep = " - ")) |> 
        set_names(paste(x, str_replace(right_vars, "_q3", ""), sep = "_"))})
  }) |> 
  relocate(geometry, .after = last_col()) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("climate_risk-DA_building")


# Add recipes -------------------------------------------------------------





recipe_CT <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "CT": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-CT",
        "minzoom": 3,
        "maxzoom": 12,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 12 ], 1, 4 
          ]
        }
      }
    }
  },
  "name": "climate_risk-CT"
}
'

recipe_DA <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "DA_empty": {
        "source": "mapbox://tileset-source/sus-mcgill/DA_empty",
        "minzoom": 3,
        "maxzoom": 8
      },
      "DA": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-DA",
        "minzoom": 9,
        "maxzoom": 13,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 13 ], 1, 4 
          ]
        },
        "tiles": {
          "layer_size": 2500
        }
      }
    }
  },
  "name": "climate_risk-DA"
}
'

recipe_building <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "DA_building_empty": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-DA_building_empty",
        "minzoom": 3,
        "maxzoom": 8
      },
      "DA_building": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-DA_building",
        "minzoom": 9,
        "maxzoom": 12
      },
      "building": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-building",
        "minzoom": 13,
        "maxzoom": 16,
        "tiles": {
          "layer_size": 2500
        }
      }
    }
  },
  "name": "climate_risk-building"
}
'

recipe_auto_zoom <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "borough": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-borough",
        "minzoom": 2,
        "maxzoom": 10
      },
      "CT": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-CT",
        "minzoom": 11,
        "maxzoom": 12
      },
       "DA": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-DA",
        "minzoom": 13,
        "maxzoom": 15
      },
       "building": {
        "source": "mapbox://tileset-source/sus-mcgill/climate_risk-building",
        "minzoom": 16,
        "maxzoom": 16,
        "tiles": {
          "layer_size": 2500
        }
      }
    }
  },
  "name": "climate_risk-auto_zoom"
}
'


# Create and publish tilesets ---------------------------------------------



create_tileset("climate_risk-borough", recipe_borough)
publish_tileset("climate_risk-borough")

create_tileset("climate_risk-CT", recipe_CT)
publish_tileset("climate_risk-CT")

create_tileset("climate_risk-DA", recipe_DA)
publish_tileset("climate_risk-DA")

create_tileset("climate_risk-building", recipe_building)
publish_tileset("climate_risk-building")

create_tileset("climate_risk-auto_zoom", recipe_auto_zoom)
publish_tileset("climate_risk-auto_zoom")
