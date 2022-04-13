#### GET DF ####################################################################

get_df <- function(tile, zoom_string) {
  if (!is.null(sus_bookmark$df)) return(sus_bookmark$df)
  print(sus_link$df)
  if (!is.null(sus_link$df)) return(sus_link$df)
  if (tile == "auto_zoom") zoom_string else tile
}
