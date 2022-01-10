#### MAP CHANGE FUNCTION #######################################################

#' @param id Namespace id of the map to redraw, likely to be `NS(id, "map")`
#' @param x A reactive sf expression containing a data frame, with the 
#' geometries to draw on the map. 
#' @param df A reactive which resolves to a character string representing the
#' underlying data set to be loaded. Currently available options are 
#' `c("borough", "building", "CT", "DA", "grid", "street")`, and it is useful
#' to decide the `mapdeck::stroke_width` on polygons map.
#' @param selection A reactive which resolves to a character string giving the 
#' ID of a row in the input data frame (`x`) which has been selected.
#' @param legend A mapdeck manual legend to be placed at the bottom right of the 
#' map. The output of a `mapdeck::mapdeck_legend` and `mapdeck::legend_element` 
#' combination.
#' @param polygons_to_clear A strings vector indicating the specific layer_id 
#' of the map to be cleared every time the map is redrawn..
#' @param standard_width A reactive logical scalar. Should the polygon width be 
#' 0 instead?
#' @return An update version of the mapdeck map.

map_change <- function(id_map, x, df, selection = reactive(NULL), 
                       legend = NULL,  polygons_to_clear = NULL, 
                       standard_width = reactive(TRUE),
                       legend_selection = reactive(NULL)) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(selection))
  stopifnot(is.reactive(standard_width))
  stopifnot(is.reactive(legend_selection))
  
  geom_type <- reactive({
    map_chr(as.character(unique(st_geometry_type(x()))), ~{
      switch(.x,  
             "POLYGON" = "polygon",
             "MULTIPOLYGON" = "polygon",
             "LINESTRING" = "line",
             "MULTILINESTRING" = "line",
             "MULTIPOINT" = "point",
             "POINT" = "point",
             "error")
    }) |> unique()
  })
  
  observeEvent({legend_selection()
    x()
    standard_width()
    df()
    geom_type()}, {
      
      # Used at all geometries:
      update_and_clean <- function() {
        mapdeck_update(map_id = id_map)  |>
          clear_polygon() |>
          clear_scatterplot() |> 
          clear_heatmap() |> 
          clear_path()
      }
      
      # Clear layer_ids fed with polygons_to_clear
      purrr::walk(polygons_to_clear, ~{
        mapdeck_update(map_id = id) |> clear_polygon(.x)
      })
      
      # Error handling
      if (geom_type() == "error") stop("`geom_type` invalid in `map_change`.")
      
      # Buildings should be extruded
      if (df() == "building") {
        update_and_clean() |> 
          add_polygon(data = x(),
                      update_view = FALSE, id = "ID", elevation = 5, 
                      fill_colour = "fill", auto_highlight = TRUE, 
                      highlight_colour = "#FFFFFF90")
        
      } else if (geom_type() == "polygon") {
        
        width <- switch(df(), "borough" = 10, "CT" = 5, "grid" = 0, 2)
        if (!standard_width()) width <- 0
        
        # Legend selection
        if (!is.null(legend_selection())) {
          sel <- 
            x() |> 
            mutate(fill = case_when(
              str_detect(fill, 
                         paste0(legend_selection(), "..$", 
                                collapse = "|")) ~ fill,
              TRUE ~ str_replace(fill, "..$", "50")))
          
          update_and_clean() |> 
            add_polygon(
              data = sel, stroke_width = width,
              stroke_colour = "#FFFFFF", fill_colour = "fill",
              update_view = FALSE, id = "ID", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90")
        } else {
          update_and_clean() |> 
            add_polygon(
              data = x(), stroke_width = width,
              stroke_colour = "#FFFFFF", fill_colour = "fill",
              update_view = FALSE, id = "ID", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90")
        }
        
      } else if (geom_type() == "line") {
        
        update_and_clean() |>
          add_path(data = x(),
                   update_view = FALSE, id = "ID", stroke_width = 5,
                   stroke_colour = "fill", auto_highlight = TRUE,
                   highlight_colour = "#FFFFFF90")
        
      } else if (geom_type() == "point") {
        
        if (df() != "street") {
          update_and_clean() |>
            add_heatmap(data = x(), update_view = FALSE,
                        colour_range = c("#AECBB4", "#91BD9A", "#73AE80",
                                         "#70999B", "#6E8EA8", "#6C83B5"),
                        intensity = 2)
          
        } else {
          
          update_and_clean() |>
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
        if (geom_type() == "polygon") {
          if (!is.null(selection())) {
            if (!is.na(selection())) {
              width <- switch(df(), "borough" = 100, "CT" = 10, 2)
              data_to_add <-
                x() |>
                filter(ID == selection()) |>
                mutate(fill = substr(fill, 1, 7))
              
              mapdeck_update(map_id = id) |>
                add_polygon(
                  data = data_to_add, elevation = 5, fill_colour = "fill",
                  update_view = FALSE, layer_id = "poly_highlight",
                  auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
            } else {
              mapdeck_update(map_id = id) |>
                clear_polygon(layer_id = "poly_highlight")
            }
          }
        }})
    })}
