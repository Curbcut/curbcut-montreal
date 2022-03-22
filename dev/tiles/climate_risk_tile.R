#### CLIMATE_RISK TILE PROCESSING ##############################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
grid <- qread("data/grid.qs")
building <- qread("data/building.qs")
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


# Process grid then upload tile source ------------------------------------

grid |> 
  as_tibble() |> 
  select(ID, name, all_of(left_vars), geometry) |> 
  mutate(across(all_of(left_vars), ~paste0("q5_", .x))) |> 
  bind_cols({
    map_dfc(left_vars, \(x) {
      map_dfc(right_vars, \(y) 
              paste(grid[[paste0(x, "_q3")]], grid[[y]], sep = " - ")) |> 
        set_names(paste(x, str_replace(right_vars, "_q3", ""), sep = "_"))})
  }) |> 
  relocate(geometry, .after = last_col()) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("climate_risk-grid")


# Process borough then upload tile source ---------------------------------

borough |> 
  as_tibble() |> 
  select(ID, name, all_of(left_vars_census), geometry) |> 
  mutate(across(all_of(left_vars_census), ~paste0("q5_", .x))) |> 
  bind_cols({
    map_dfc(left_vars, \(x) {
      map_dfc(right_vars, \(y) 
              paste(borough[[paste0(x, "_q3")]], borough[[y]], sep = " - ")) |> 
        set_names(paste(x, str_replace(right_vars, "_q3", ""), sep = "_"))
    })}) |> 
  relocate(geometry, .after = last_col()) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("climate_risk-borough")


# Process CT then upload tile source ---------------------------------

CT |> 
  as_tibble() |> 
  select(ID, name, all_of(left_vars_census), geometry) |> 
  mutate(across(all_of(left_vars_census), ~paste0("q5_", .x))) |> 
  bind_cols({
    map_dfc(left_vars, \(x) {
      map_dfc(right_vars, \(y) 
              paste(CT[[paste0(x, "_q3")]], CT[[y]], sep = " - ")) |> 
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
  upload_tile_source("climate_risk-DA")


# Process building then upload tile source --------------------------------

building_to_process <- 
  building |> 
  as_tibble() |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"), 
                ~paste(canale_ind_q3_2016, .x, sep = " - "))) |> 
  relocate(canale_ind_q5_2016, .after = name) |> 
  select(-canale_ind_q3_2016) |> 
  rename_with(~paste0("canale_ind_2016_", str_remove(.x, "_q3")),
              contains("_q3")) |> 
  rename(canale_ind_2016 = canale_ind_q5_2016) |> 
  mutate(canale_ind_2016 = paste0("q5_", canale_ind_2016)) |> 
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
                'sus-mcgill/canale-building?access_token=', .sus_token, 
                '" -F file=@', tmp, 
                ' --header "Content-Type: multipart/form-data"')
  system(out)
  
})


# Process DA_building then upload tile source -----------------------------

DA |> 
  st_set_geometry("building") |> 
  as_tibble() |> 
  select(ID, name, all_of(vars_to_add), geometry = building) |> 
  mutate(across(contains("_q3"), 
                ~paste(canale_ind_q3_2016, .x, sep = " - "))) |> 
  relocate(canale_ind_q5_2016, .after = name) |> 
  select(-canale_ind_q3_2016) |> 
  rename_with(~paste0("canale_ind_2016_", str_remove(.x, "_q3")),
              contains("_q3")) |> 
  rename(canale_ind_2016 = canale_ind_q5_2016) |> 
  mutate(canale_ind_2016 = paste0("q5_", canale_ind_2016)) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("canale-DA_building", "sus-mcgill", .sus_token)


# Process DA_building_empty then upload tile source -----------------------

DA |> 
  st_set_geometry("building") |> 
  select(ID, name, geometry = building) |> 
  upload_tile_source("canale-DA_building_empty", "sus-mcgill", .sus_token)


# Add recipes -------------------------------------------------------------

recipe_borough <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "borough": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-borough",
        "minzoom": 3,
        "maxzoom": 11,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 10 ], 1, 4 
          ]
        }
      }
    }
  },
  "name": "canale-borough"
}
'

recipe_CT <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "CT": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-CT",
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
  "name": "canale-CT"
}
'

recipe_DA <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "DA_empty": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA_empty",
        "minzoom": 3,
        "maxzoom": 8
      },
      "DA": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA",
        "minzoom": 9,
        "maxzoom": 13,
        "features": {
           "simplification": 1
        }
      }
    }
  },
  "name": "canale-DA"
}
'

recipe_building <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "DA_building_empty": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA_building_empty",
        "minzoom": 3,
        "maxzoom": 8
      },
      "DA_building": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA_building",
        "minzoom": 9,
        "maxzoom": 12
      },
      "building": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-building",
        "minzoom": 13,
        "maxzoom": 16,
        "tiles": {
          "layer_size": 2500
        }
      }
    }
  },
  "name": "canale-building"
}
'

recipe_auto_zoom <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "borough": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-borough",
        "minzoom": 2,
        "maxzoom": 10
      },
      "CT": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-CT",
        "minzoom": 11,
        "maxzoom": 12
      },
       "DA": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA",
        "minzoom": 13,
        "maxzoom": 15
      },
       "building": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-building",
        "minzoom": 16,
        "maxzoom": 16
      }
    }
  },
  "name": "canale-auto_zoom"
}
'


# Create and publish tilesets ---------------------------------------------

create_tileset("canale-borough", recipe_borough, "sus-mcgill", .sus_token)
publish_tileset("canale-borough", "sus-mcgill", .sus_token)

create_tileset("canale-CT", recipe_CT, "sus-mcgill", .sus_token)
publish_tileset("canale-CT", "sus-mcgill", .sus_token)

create_tileset("canale-DA", recipe_DA, "sus-mcgill", .sus_token)
publish_tileset("canale-DA", "sus-mcgill", .sus_token)

create_tileset("canale-building", recipe_building, "sus-mcgill", .sus_token)
publish_tileset("canale-building", "sus-mcgill", .sus_token)

create_tileset("canale-auto_zoom", recipe_auto_zoom, "sus-mcgill", .sus_token)
publish_tileset("canale-auto_zoom", "sus-mcgill", .sus_token)




# Recipe for building testing ---------------------------------------------

recipe_building_test <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "DA_building_empty": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA_building_empty",
        "minzoom": 3,
        "maxzoom": 8,
        "tiles": {
          "bbox": [ -73.57, 45.50, -73.56, 45.51 ]
        }
      },
      "DA_building": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA_building",
        "minzoom": 9,
        "maxzoom": 12,
        "tiles": {
          "bbox": [ -73.57, 45.50, -73.56, 45.51 ]
        }
      },
      "building": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-building",
        "minzoom": 13,
        "maxzoom": 16,
        "tiles": {
          "bbox": [ -73.57, 45.50, -73.56, 45.51 ]
        }
      }
    }
  },
  "name": "canale-building_test"
}
'


