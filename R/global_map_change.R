#' function to change map between polygons (borough, CT, DA, ...), lines (street)
#' and points

map_change <- function(id, x, zoom, map_stroke_width = NULL) {

  stopifnot(is.reactive(x))
  stopifnot(is.reactive(zoom))
  
  geom_type <- as.character(unique(st_geometry_type(x())))
  geom_type <-  switch(geom_type,
                       "POLYGON" = "polygon",
                       "MULTIPOLYGON" = "polygon",
                       "LINESTRING" = "line",
                       "MULTILINESTRING" = "line",
                       "MULTIPOINT" = "point",
                       "POINT" = "point",
                       "error")
  
  # Error handling
  if (geom_type == "error") stop("`geom_type` is invalid in `map_change`.")

  # Buildings should be extruded
  if (zoom() == "building") {
    mapdeck_update(map_id = id)  %>%
      clear_polygon() %>%
      clear_path() %>%
      add_sf(data = x(),
             update_view = FALSE, id = "ID", elevation = 5, 
             fill_colour = "fill", auto_highlight = TRUE, 
             highlight_colour = "#FFFFFF90")
    
  } else if (geom_type == "line") {

    mapdeck_update(map_id = id)  %>%
      clear_polygon() %>%
      add_sf(data = x(),
             update_view = FALSE, id = "ID", stroke_width = 5,
             stroke_colour = "fill", auto_highlight = TRUE,
             highlight_colour = "#FFFFFF90")

  } else if (geom_type == "polygon") {

    if (is.null(map_stroke_width)) {
      width <- switch(zoom(), "borough" = 100, "CT" = 10, "DA" = 2, 2)
    } else width <- map_stroke_width

    mapdeck_update(map_id = id) %>%
      clear_path() %>%
      add_sf(
        data = x(), stroke_width = width,
        stroke_colour = "#FFFFFF", fill_colour = "fill",
        update_view = FALSE, id = "ID", auto_highlight = TRUE,
        highlight_colour = "#FFFFFF90")

  }

}