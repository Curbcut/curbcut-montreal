#' function to change map between polygons (borough, CT, DA, ...), lines (street)
#' and points

map_change <- function(id, x, zoom, map_stroke_width = NULL) {

  stopifnot(is.reactive(x))
  stopifnot(is.reactive(zoom))
  
  geom_type <-  switch(as.character(unique(st_geometry_type(x()))),
                       "MULTIPOLYGON" = "polygon",
                       "LINESTRING" = "line") # add points for future

  if (geom_type == "line") {

    mapdeck_update(map_id = id)  %>%
      clear_polygon() %>%
      add_sf(data = x(),
             update_view = FALSE, id = "ID", stroke_width = 5,
             stroke_colour = "fill", auto_highlight = TRUE,
             highlight_colour = "#FFFFFF90")

  } else {

    if (is.null(map_stroke_width)) {width <- switch(zoom(), 
                                                  "borough" = 100, 
                                                  "CT" = 10, 
                                                  "DA" = 2)
    # At the moment we don't use other maps than census, but maybe at
    # some point we'll want to project other stuff. Something like that, with
    # some repair because I can't feed a list to switch, could be great:
    } else width <- switch(zoom(), map_stroke_width)

    mapdeck_update(map_id = id) %>%
      clear_path() %>%
      add_sf(
        data = x(), stroke_width = width,
        stroke_colour = "#FFFFFF", fill_colour = "fill",
        update_view = FALSE, id = "ID", auto_highlight = TRUE,
        highlight_colour = "#FFFFFF90")

  }

}