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
  "building" = "Building",
  "street" = "Street",
  "heatmap" = "Heatmap",
  "point" = "Point",
  "centraide" = "Centraide",
  USE.NAMES = FALSE)

get_zoom_label <- function(zoom_levels) {
  zl <- names(sort(zoom_levels))
  zl <- get_zoom_name(zl)
  return(zl)
}

get_zoom_label_t <- function(zoom_levels, r) {
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
  if (x == "Centraide") return("centraide")
}

# Get the right `map_zoom_levels_x`.
#' @param default is a string corresponding to an available `map_zoom_levels_x`
#' initiated in global.R. The default is `CMA`, for `map_zoom_levels_CMA`
#' @param geo is a string corresponding to `r$geo()`, user decides what zoom
#' levels they desire.
get_zoom_levels <- function(default = "CMA", geo, var_left) {
  get_z <- function(z) {
    out <- get0(paste0("map_zoom_levels_", z))
    if (!is.null(out)) return(out)
    stop(paste0("`map_zoom_levels_", z, "` does not exist as a zoom level"))
  }
  zooms_default <- get_z(default)
  zooms_geo <- get_z(geo)
  
  if (default == geo) return(list(levels = zooms_default, scale = geo))
  
  # Check if the main variable is part of the set of variables in the
  # prefered zoom levels. If it is, return the wanted map_zoom_levels_CMA
  if (var_left[1] %in% names(get0(names(zooms_geo[1])))) 
    return(list(levels = zooms_geo, scale = geo))
  
  return(list(levels = zooms_default, scale = geo))
}
