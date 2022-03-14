#### CANALE TILE PROCESSING ####################################################

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
  upload_tile_source("canale-borough3", "dwachsmuth", access_token)


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
  upload_tile_source("canale-CT", "dwachsmuth", access_token)


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
  upload_tile_source("canale-DA", "dwachsmuth", access_token)



# Add recipes -------------------------------------------------------------

recipe_borough <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "borough": {
        "source": "mapbox://tileset-source/dwachsmuth/canale-borough3",
        "minzoom": 2,
        "maxzoom": 5
      }
    }
  },
  "name": "canale-borough3"
}
'

recipe_autzoom <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "borough": {
        "source": "mapbox://tileset-source/dwachsmuth/canale-borough3",
        "minzoom": 2,
        "maxzoom": 5
      },
      "CT": {
        "source": "mapbox://tileset-source/dwachsmuth/canale-CT",
        "minzoom": 6,
        "maxzoom": 8
      },
       "DA": {
        "source": "mapbox://tileset-source/dwachsmuth/canale-DA",
        "minzoom": 9,
        "maxzoom": 10
      }

    }
  },
  "name": "canale-autozoom-test-1"
}
'


# Create and publish tileset ----------------------------------------------

create_tileset("canale-borough3", recipe, "dwachsmuth", access_token)
publish_tileset("canale-borough3", "dwachsmuth", access_token)

create_tileset("canale-autozoom-test-1", recipe_autzoom, "dwachsmuth", access_token)
publish_tileset("canale-autozoom-test-1", "dwachsmuth", access_token)

