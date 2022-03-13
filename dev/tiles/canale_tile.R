#### CANALE TILE PROCESSING ####################################################

# Get variables to add ----------------------------------------------------

vars_to_add <- 
  variables |> 
  filter(source == "census") |> 
  filter(theme != "Employment", !is.na(theme)) |> 
  pull(var_code) |> 
  c("canale_ind") |> 
  paste0("_q5_2016")


# Process borough then upload tile source ---------------------------------

borough |> 
  select(ID, name, all_of(vars_to_add), geometry) |> 
  upload_tile_source("canale-borough", "dwachsmuth", access_token)



# Add recipe --------------------------------------------------------------

recipe <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "borough": {
        "source": "mapbox://tileset-source/dwachsmuth/canale-borough",
        "minzoom": 5,
        "maxzoom": 10
      }
    }
  },
  "name": "canale-borough"
}
'

# Create and publish tileset ----------------------------------------------

create_tileset("canale-borough", recipe, "dwachsmuth", access_token)
publish_tileset("canale-borough", "dwachsmuth", access_token)
