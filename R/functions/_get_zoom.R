#### ZOOM FUNCTIONS ############################################################

get_zoom <- function(zoom) floor(zoom * 2) / 2

get_zoom_string <- function(zoom, zoom_levels, geo = "CMA") {
  zoom_levels <- sort(zoom_levels)
  out <- names(zoom_levels)[zoom >= zoom_levels]
  out <- out[length(out)]
  out <- paste(geo, out, sep = "_")
  return(out)
}

get_zoom_name <- function(x) {
  sapply(gsub(".*_", "", x), \(z) {
    scales_dictionary$slider_title[scales_dictionary$scale == z]
  }, USE.NAMES = FALSE)
}

get_zoom_label <- function(zoom_levels) {
  zl <- names(sort(zoom_levels))
  zl <- get_zoom_name(zl)
  return(zl)
}

get_zoom_label_t <- function(zoom_levels, r) {
  zl <- names(sort(zoom_levels))
  zl <- sapply(get_zoom_name(zl), cc_t, r = r, USE.NAMES = FALSE)
  return(zl)
}

get_zoom_code <- function(x) {
  translated <- na.omit(translation_fr$en[translation_fr$fr == x])
  
  sub_vec <- 
    if (length(translated) > 0) {
      scales_dictionary$slider_title == x | 
        scales_dictionary$slider_title == translated
    } else scales_dictionary$slider_title == x

  scales_dictionary$scale[sub_vec]
}

# Get the right `map_zoom_levels_x`.
#' @param default is a string corresponding to an available `map_zoom_levels_x`
#' initiated in global.R. The default is `CMA`, for `map_zoom_levels_CMA`
#' @param geo is a string corresponding to `r$geo()`, user decides what zoom
#' levels they desire.
get_zoom_levels <- function(default = "CMA", geo, var_left, 
                            suffix_zoom_levels = "") {
  get_z <- function(z) {
    out <- get0(paste0("map_zoom_levels_", z))
    if (!is.null(out)) return(out)
    stop(paste0("`map_zoom_levels_", z, "` does not exist as a zoom level"))
  }
  zooms_default <- get_z(paste0(default, suffix_zoom_levels))
  zooms_geo <- get_z(paste0(geo, suffix_zoom_levels))
  
  if (default == geo) return(list(levels = zooms_default, region = geo))
  
  # Check if the main variable is part of the set of variables in the
  # prefered zoom levels. If it is, return the wanted map_zoom_levels
  geo_scale <- paste(geo, names(zooms_geo[1]), sep = "_")
  
  vl_in_sql <- var_left[1] %in% tables_in_sql[[geo_scale]]
    
  exists_at_geo_scale <- var_left[1] %in% names(get0(geo_scale)) || vl_in_sql
  if (exists_at_geo_scale) return(list(levels = zooms_geo, region = geo))
  
  return(list(levels = zooms_default, region = default))
}
