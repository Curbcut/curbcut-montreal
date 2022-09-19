#### GET DF ####################################################################

get_df <- function(tile, zoom_string) {
  out <- if (grepl("auto_zoom", tile)) zoom_string else tile
  gsub("^.*-", "", out)
}
