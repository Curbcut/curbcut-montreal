#### GET DF ####################################################################

get_df <- function(tile, zoom, zoom_levels) {
  if (tile == "auto_zoom") get_zoom_string(zoom, zoom_levels) else tile
}
