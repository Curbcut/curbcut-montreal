#### ZOOM MODULE GLOBALS #######################################################

get_zoom <- function(zoom, zoom_levels) {
  zoom_levels <- sort(zoom_levels)
  out <- names(zoom_levels)[zoom >= zoom_levels]
  out <- out[length(out)]
  return(out)
}

get_zoom_name <- function(x) sapply(
  x, 
  switch, 
  "borough" = "Borough/city",
  "CT" = "Census tract",
  "DA" = "Dissemination area",
  "building" = "Building",
  "street" = "Street",
  "heatmap" = "Heatmap",
  "point" = "Point",
  USE.NAMES = FALSE)

get_zoom_label <- function(zoom_levels) {
  zl <- names(sort(zoom_levels))
  zl <- get_zoom_name(zl)
  return(zl)
}

get_zoom_code <- function(x) sapply(
  x, 
  switch,
  "Borough/city" = "borough",
  "Census tract" = "CT",
  "Dissemination area" = "DA",
  "Building" = "building",
  "Street" = "street",
  "Heatmap" = "heatmap",
  "Point" = "point",
  USE.NAMES = FALSE)
