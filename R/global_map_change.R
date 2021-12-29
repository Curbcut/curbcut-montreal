#' function to change map between polygons (borough, CT, DA, ...), 
#' lines (street) and points (crash)

map_change <- function(id_map, df, zoom, legend = NULL, polygons_to_clear = NULL,
                       overthrow_width = FALSE) {
  
  stopifnot(
    is.reactive(df),
    is.reactive(zoom)
  )
  
  geom_type <-  switch(as.character(unique(st_geometry_type(df()))),
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
  if (zoom() == "building") {
    update_and_clean() %>% 
      add_polygon(data = df(),
                  update_view = FALSE, id = "ID", elevation = 5, 
                  fill_colour = "fill", auto_highlight = TRUE, 
                  highlight_colour = "#FFFFFF90")
    
  } else if (geom_type == "polygon") {

    width <- switch(zoom(), "borough" = 100, "CT" = 10, "DA" = 2, "grid" = 0, 2)
    if (overthrow_width) width <- 0
    
    update_and_clean() %>% 
      add_polygon(
        data = df(), stroke_width = width,
        stroke_colour = "#FFFFFF", fill_colour = "fill",
        update_view = FALSE, id = "ID", auto_highlight = TRUE,
        highlight_colour = "#FFFFFF90")
    
    
  } else if (geom_type == "line") {
    
    update_and_clean() %>%
      add_path(data = df(),
             update_view = FALSE, id = "ID", stroke_width = 5,
             stroke_colour = "fill", auto_highlight = TRUE,
             highlight_colour = "#FFFFFF90")
    
  } else if (geom_type == "point") {
    
    if (zoom() != "street") {
      update_and_clean() %>%
        add_heatmap(data = df(), update_view = FALSE,
                    colour_range = c("#AECBB4", "#91BD9A", "#73AE80",
                                     "#70999B", "#6E8EA8", "#6C83B5"),
                    intensity = 2)
      
    } else {
      
      update_and_clean() %>%
        add_scatterplot(data = df(), update_view = FALSE,
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
}