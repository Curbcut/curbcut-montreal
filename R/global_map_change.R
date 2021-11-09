#' function to change map between polygons (borough, CT, DA, ...), 
#' lines (street) and points (crash)

map_change <- function(id_map, df, zoom, legend = NULL) {
  
  geom_type <-  switch(as.character(unique(st_geometry_type(df))),
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
    mapdeck_update(map_id = id_map)  %>%
      clear_polygon() %>%
      clear_path() %>%
      add_polygon(data = df,
             update_view = FALSE, id = "ID", elevation = 5, 
             fill_colour = "fill", auto_highlight = TRUE, 
             highlight_colour = "#FFFFFF90")

  } else if (geom_type == "polygon") {

    width <- switch(zoom(), "borough" = 100, "CT" = 10, 2)
    
    mapdeck_update(map_id = id_map) %>%
      add_polygon(
        data = df, stroke_width = width,
        stroke_colour = "#FFFFFF", fill_colour = "fill",
        update_view = FALSE, id = "ID", auto_highlight = TRUE,
        highlight_colour = "#FFFFFF90") %>% 
      clear_heatmap() %>% 
      clear_pointcloud()
    

  } else if (geom_type == "point") {

    if (zoom() != "DA") {
      mapdeck_update(map_id = id_map) %>%
        add_heatmap(data = df, update_view = FALSE,
                    colour_range = c("#AECBB4", "#91BD9A", "#73AE80",
                                     "#70999B", "#6E8EA8", "#6C83B5"),
                    intensity = 2) %>%
        clear_polygon() %>%
        clear_pointcloud()

    } else {
      
      mapdeck_update(map_id = id_map) %>%
        add_pointcloud(data = df, update_view = FALSE,
                       id = "ID",
                       auto_highlight = TRUE,
                       highlight_colour = "#FFFFFF90",
                       fill_colour = "fill",
                       fill_opacity = 200,
                       legend = legend,
                       # tooltip = "label",
                       radius = 10) %>%
        clear_polygon() %>%
        clear_heatmap()
    }
  }
}