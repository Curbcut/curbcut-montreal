#### GET DF ####################################################################

get_df <- function(tile, zoom_string, r = r) {
  if (!is.null(r$sus_bookmark$df)) return(r$sus_bookmark$df)
  if (!is.null(r$sus_link$df)) return(r$sus_link$df)
  if (tile == "auto_zoom") zoom_string else tile
}
