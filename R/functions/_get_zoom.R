#### ZOOM FUNCTIONS ############################################################

get_zoom <- function(zoom) floor(zoom * 2) / 2

get_zoom_string <- function(zoom, zoom_levels) {
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
  "grid" = "250-m grid cell",
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

get_zoom_label_t <- function(zoom_levels, r = r) {
  zl <- names(sort(zoom_levels))
  zl <- sapply(get_zoom_name(zl), sus_translate, r = r, USE.NAMES = FALSE)
  return(zl)
}

get_zoom_code <- function(x) {
  if (x == "Borough/city" || x == "Arrondissement/ville") return("borough")
  if (x == "Census tract" || x == "Secteur de recensement") return("CT")
  if (x == "Dissemination area" || x == "Aire de diffusion") return("DA")
  if (x == "Building" || x == "Bâtiment") return("building")
  if (x == "Street" || x == "Rue") return("street")
  if (x == "Heatmap") return("heatmap")
  if (x == "Point") return("point")
}
