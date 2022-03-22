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
  "borough" = sus_translate("Borough/city"),
  "CT" = sus_translate("Census tract"),
  "DA" = sus_translate("Dissemination area"),
  "building" = sus_translate("Building"),
  "street" = sus_translate("Street"),
  "heatmap" = sus_translate("Heatmap"),
  "point" = sus_translate("Point"),
  USE.NAMES = FALSE)

get_zoom_label <- function(zoom_levels) {
  zl <- names(sort(zoom_levels))
  zl <- get_zoom_name(zl)
  return(zl)
}

get_zoom_code <- function(x) {
  
  x <- 
    if (sus_rv$lang() == "fr") {
      z <- translation_fr[translation_fr$fr == x, ]$en
      z[!is.na(z)]
    } else x
  
  sapply(
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

}
