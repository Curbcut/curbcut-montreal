#### MAP CHANGE FUNCTION #######################################################

#' @param map_id Namespace id of the map to redraw, likely to be `NS(id, "map")`
#' @return An update version of the rdeck map.

rdeck_change <- function(id, map_id, tile, map_var, select_id) {
  
  ## Setup ---------------------------------------------------------------------
  
  # Error checking
  stopifnot(is.reactive(tile))
  stopifnot(is.reactive(map_var))
  stopifnot(is.reactive(select_id))
  
  
  
  moduleServer(id, function(input, output, session) {
    
    ## Update map on data change -------------------------------------------------
    
    observe({
        print("happens?")
        
        rdeck_proxy(map_id) |>
          add_mvt_layer(
            id = id, auto_highlight = TRUE,
            highlight_color = "#FFFFFF80", pickable = TRUE,
            get_fill_color = scale_fill_sus(rlang::sym(map_var()), "EE"),
            get_line_color = "#FFFFFF", line_width_units = "pixels", 
            get_line_width = 1)
        
      })
    
  })
}
