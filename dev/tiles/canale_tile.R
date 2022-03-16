#### CANALE TILE PROCESSING ####################################################

library(tidyverse)
library(sf)
library(qs)
qload("data/census.qsm")
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
  mutate(canale_ind_2016 = paste0("q5_", canale_ind_2016)) |> 
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
  mutate(canale_ind_2016 = paste0("q5_", canale_ind_2016)) |> 
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
  mutate(canale_ind_2016 = paste0("q5_", canale_ind_2016)) |> 
  st_as_sf() |> 
  st_set_agr("constant") |> 
  upload_tile_source("canale-DA", "sus-mcgill", .sus_token)



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
      "DA": {
        "source": "mapbox://tileset-source/sus-mcgill/canale-DA",
        "minzoom": 3,
        "maxzoom": 13
      }
    }
  },
  "name": "canale-DA"
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
        "maxzoom": 13,
        "features": {
          "simplification": 3
        }
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

create_tileset("canale-auto_zoom", recipe_auto_zoom, "sus-mcgill", .sus_token)
publish_tileset("canale-auto_zoom", "sus-mcgill", .sus_token)

