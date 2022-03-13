#### MAPBOX TILES ##############################################################


# Add simple borough tilset source ----------------------------------------

borough |> 
  select(ID, name, contains(c("_q5_2016"))) |> 
  select(ID, name, contains(c("canale", "housing"))) |> 
  mutate(across(where(is.numeric), replace_na, 99)) |> 
  select(1:4) |> 
  upload_tile_source("borough_5", "dwachsmuth", access_token)


# Add recipe --------------------------------------------------------------

recipe <- '
{
  "recipe": {
    "version": 1,
    "layers": {
      "borough": {
        "source": "mapbox://tileset-source/dwachsmuth/borough_5",
        "minzoom": 6,
        "maxzoom": 10
      }
    },
    "tiles": {
      "remove_filled": true
    }
  },
  "name": "borough_5"
}
'


# Create and publish tileset ----------------------------------------------

create_tileset("borough_5", recipe, "dwachsmuth", access_token)
# update_tileset("borough_1", recipe, "dwachsmuth", access_token)
publish_tileset("borough_5", "dwachsmuth", access_token)



# Create style ------------------------------------------------------------

style <- list(
  version = 8,
  name = "borough_canale_ind_2016",
  metadata = "",
  sources = list(
    borough = list(
      url = "mapbox://dwachsmuth.borough_1",
      type = "vector"
    )
  ),
  layers = list(
    list(
      id = "borough_canale_ind_2016",
      type = "fill",
      source = "borough_canale_ind_2016",
      `source-layer` = "borough",
      paint = list(
        `fill-outline-color` = "hsl(0, 0%, 100%)",
        `fill-color` = "hsl(202, 50%, 100%)"
      )
    )
  )
)


test <- create_style(jsonlite::toJSON(style), "dwachsmuth", access_token)
test |> content()