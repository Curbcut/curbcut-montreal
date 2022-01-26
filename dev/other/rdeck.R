#### RDECK EXPERIMENTATION #####################################################

# remotes::install_github("anthonynorth/rdeck")

library(rdeck)

rdeck(
  map_style = mapbox_light(),
  initial_bounds = st_bbox(building)
  ) |> 
  add_solid_polygon_layer(
    data = st_set_geometry(DA, "building"),
    visible = NULL,
    pickable = TRUE,
    get_polygon = building
  )
