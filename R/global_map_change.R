#' function to change map between polygons (borough, CT, DA, ...), 
#' lines (street) and points (crash)

map_change <- function(id_map, x, df, selection = reactive(NULL), 
                       legend = NULL,  polygons_to_clear = NULL, 
                       standard_width = reactive(TRUE)) {
  
  stopifnot(
    is.reactive(x),
    is.reactive(df)
  )
  
  observeEvent({x()
    df()
    standard_width()}, {
  
  geom_type <-  switch(as.character(unique(st_geometry_type(x()))),
                       "POLYGON" = "polygon",
                       "MULTIPOLYGON" = "polygon",
                       "LINESTRING" = "line",
                       "MULTILINESTRING" = "line",
                       "MULTIPOINT" = "point",
                       "POINT" = "point",
                       "error")
  
  # Used at all geometries:
  update_and_clean <- function() {
    mapdeck_update(map_id = id_map)  %>%
      clear_polygon() %>%
      clear_scatterplot() %>% 
      clear_pointcloud() %>% 
      clear_path()
  }
  
  # Clear layer_ids fed with polygons_to_clear
  purrr::walk(polygons_to_clear, ~mapdeck_update(map_id = id_map) %>% clear_polygon(.x))
  
  # Error handling
  if (geom_type == "error") stop("`geom_type` is invalid in `map_change`.")
  
  # Buildings should be extruded
  if (df() == "building") {
    update_and_clean() %>% 
      add_polygon(data = x(),
                  update_view = FALSE, id = "ID", elevation = 5, 
                  fill_colour = "fill", auto_highlight = TRUE, 
                  highlight_colour = "#FFFFFF90")
    
  } else if (geom_type == "polygon") {

    width <- switch(df(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
    if (!standard_width()) width <- 0
    
    update_and_clean() %>% 
      add_polygon(
        data = x(), stroke_width = width,
        stroke_colour = "#FFFFFF", fill_colour = "fill",
        update_view = FALSE, id = "ID", auto_highlight = TRUE,
        highlight_colour = "#FFFFFF90")
    
    
  } else if (geom_type == "line") {
    
    update_and_clean() %>%
      add_path(data = x(),
             update_view = FALSE, id = "ID", stroke_width = 5,
             stroke_colour = "fill", auto_highlight = TRUE,
             highlight_colour = "#FFFFFF90")
    
  } else if (geom_type == "point") {
    
    if (df() != "street") {
      update_and_clean() %>%
        add_heatmap(data = x(), update_view = FALSE,
                    colour_range = c("#AECBB4", "#91BD9A", "#73AE80",
                                     "#70999B", "#6E8EA8", "#6C83B5"),
                    intensity = 2)
      
    } else {
      
      update_and_clean() %>%
        add_scatterplot(data = x(), update_view = FALSE,
                        id = "ID",
                        auto_highlight = TRUE,
                        highlight_colour = "#FFFFFF90",
                        fill_colour = "fill",
                        fill_opacity = 200,
                        legend = legend,
                        # tooltip = "label",
                        radius = 10,
                        radius_min_pixels = 8)
    }
  }
  
  # Update map in response to poly change
  observeEvent(selection(), {
    if (!is.null(selection())) {
    if (!is.na(selection())) {
      width <- switch(df(), "borough" = 100, "CT" = 10, 2)
      data_to_add <-
        x() |>
        filter(ID == selection()) |>
        mutate(fill = substr(fill, 1, 7))

      mapdeck_update(map_id = id_map) |>
        add_polygon(
          data = data_to_add, elevation = 5, fill_colour = "fill",
          update_view = FALSE, layer_id = "poly_highlight",
          auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
    } else {
      mapdeck_update(map_id = id_map) |>
        clear_polygon(layer_id = "poly_highlight")
    }
  }})
  
  
})}