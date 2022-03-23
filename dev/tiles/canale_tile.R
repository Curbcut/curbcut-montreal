#### CANALE TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
building <- qread("data/building_full.qs")
variables <- qread("data/variables.qs")
source("dev/tiles/tile_functions.R")


# Get variables to add ----------------------------------------------------

vars_to_add <- 
  variables |> 
  filter(source == "census") |> 
  filter(theme != "Employment", !is.na(theme)) |> 
  pull(var_code) |> 
  c("canale_ind") |> 
  paste0("_q3_2016") |> 
  c("canale_ind_q5_2016")


# Process borough then upload tile source ---------------------------------

borough |> 
  as_tibble() |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"), 
                ~paste(canale_ind_q3_2016, .x, sep = " - "))) |> 
  relocate(canale_ind_q5_2016, .after = name) |> 
  select(-canale_ind_q3_2016) |> 
  rename_with(~paste0("canale_ind_2016_", str_remove(.x, "_q3")),
              contains("_q3")) |> 
  rename(canale_ind_2016 = canale_ind_q5_2016) |> 
  mutate(canale_ind_2016 = as.integer(canale_ind_2016),
         across(c(-ID, -name, -canale_ind_2016, -geometry), trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("canale-borough", "sus-mcgill", .sus_token)


# Process CT then upload tile source ---------------------------------

CT |> 
  as_tibble() |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"), 
                ~paste(canale_ind_q3_2016, .x, sep = " - "))) |> 
  relocate(canale_ind_q5_2016, .after = name) |> 
  select(-canale_ind_q3_2016) |> 
  rename_with(~paste0("canale_ind_2016_", str_remove(.x, "_q3")),
              contains("_q3")) |> 
  rename(canale_ind_2016 = canale_ind_q5_2016) |> 
  mutate(canale_ind_2016 = as.integer(canale_ind_2016),
         across(c(-ID, -name, -canale_ind_2016, -geometry), trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("canale-CT", "sus-mcgill", .sus_token)


# Process DA then upload tile source ---------------------------------

DA |> 
  as_tibble() |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  mutate(across(contains("_q3"), 
                ~paste(canale_ind_q3_2016, .x, sep = " - "))) |> 
  relocate(canale_ind_q5_2016, .after = name) |> 
  select(-canale_ind_q3_2016) |> 
  rename_with(~paste0("canale_ind_2016_", str_remove(.x, "_q3")),
              contains("_q3")) |> 
  rename(canale_ind_2016 = canale_ind_q5_2016) |> 
  mutate(canale_ind_2016 = as.integer(canale_ind_2016),
         across(c(-ID, -name, -canale_ind_2016, -geometry), trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("canale-DA", "sus-mcgill", .sus_token)


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
  mutate(canale_ind_2016 = as.integer(canale_ind_2016),
         across(c(-ID, -name, -canale_ind_2016, -geometry), trans_var)) |> 
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
  mutate(canale_ind_2016 = as.integer(canale_ind_2016),
         across(c(-ID, -name, -canale_ind_2016, -geometry), trans_var)) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("canale-DA_building", "sus-mcgill", .sus_token)


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
        "source": "mapbox://tileset-source/sus-mcgill/DA_empty",
        "minzoom": 3,
        "maxzoom": 8
      },
      "DA": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA",
        "minzoom": 9,
        "maxzoom": 13,
        "features": {
          "simplification": [ "case",
            [ "==", [ "zoom" ], 13 ], 1, 4 
          ]
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
        "source": "mapbox://tileset-source/sus-mcgill/DA_building_empty",
        "minzoom": 3,
        "maxzoom": 8
      },
      "DA_building": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA_building",
        "minzoom": 9,
        "maxzoom": 13
      },
      "building": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-building",
        "minzoom": 14,
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

create_tileset("canale-borough", recipe_borough)
publish_tileset("canale-borough")

create_tileset("canale-CT", recipe_CT)
publish_tileset("canale-CT")

create_tileset("canale-DA", recipe_DA)
publish_tileset("canale-DA")

create_tileset("canale-building", recipe_building)
publish_tileset("canale-building")

create_tileset("canale-auto_zoom", recipe_auto_zoom)
publish_tileset("canale-auto_zoom")







# Recipe for building testing ---------------------------------------------

recipe_building_test <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "DA_building_empty": {
        "source": "mapbox://tileset-source/sus-mcgill/DA_building_empty",
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
