#### ZOOM FUNCTIONS ############################################################

get_zoom <- function(zoom) floor(zoom * 2) / 2

get_zoom_string <- function(zoom, zoom_levels, geo = "CMA") {
  zoom_levels <- sort(zoom_levels)
  out <- names(zoom_levels)[zoom >= zoom_levels]
  out <- out[length(out)]
  out <- paste(geo, out, sep = "_")
  return(out)
}
