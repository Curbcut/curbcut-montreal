#### ZOOM MODULE GLOBALS #######################################################

get_zoom <- function(zoom, zoom_levels) {
  zoom_levels <- sort(zoom_levels)
  out <- names(zoom_levels)[zoom >= zoom_levels]
  out <- out[length(out)]
  return(out)
}

get_zoom_name <- function(x) {
  x <- case_when(
    x == "borough" ~ "Borough/city",
    x == "CT" ~ "Census tract",
    x == "DA" ~ "Dissemination area",
    x == "building" ~ "Building",
    x == "street" ~ "Street")
  return(x)
}

get_zoom_label <- function(zoom_levels) {
  zl <- names(sort(zoom_levels))
  zl <- get_zoom_name(zl)
  return(zl)
}
