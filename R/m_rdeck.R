#### RDECK CHANGE SERVER #######################################################

#' @param map_id Namespace id of the map to redraw, likely to be `NS(id, "map")`
#' @return An updated version of the rdeck map.

rdeck_server <- function(id, map_id, tile, map_var, zoom, select_id) {
  
  ## Setup ---------------------------------------------------------------------
  
  # Error checking
  stopifnot(is.reactive(tile))
  stopifnot(is.reactive(map_var))
  stopifnot(is.reactive(select_id))
  stopifnot(is.reactive(zoom))
  
  moduleServer(id, function(input, output, session) {
    
  ## Update map on data change -------------------------------------------------
    
    observeEvent({
      map_var()
      select_id()
      zoom()
      }, rdeck_proxy(map_id) |>
        add_mvt_layer(
          id = id, 
          auto_highlight = TRUE, highlight_color = "#FFFFFF80", 
          pickable = if (tile() == "DA" && zoom() == "borough") FALSE else TRUE,
          get_fill_color = scale_fill_sus(rlang::sym(map_var()), "FF"),
          get_line_color = "#FFFFFF", line_width_units = "pixels", 
          get_line_width = scale_line_width_sus(select_id()),
          extruded = if (tile() == "auto_zoom" && zoom() == "building") 
            TRUE else FALSE, material = FALSE, get_elevation = 5)
    )
    
    observeEvent(tile(), {
      observe({
        rdeck_proxy(map_id) |>
          add_mvt_layer(
            id = id, data = mvt_url(paste0("sus-mcgill.canale-", tile())),
            auto_highlight = TRUE, highlight_color = "#FFFFFF80", 
            pickable = if (tile() == "DA" && zoom() == "borough") FALSE else TRUE,
            get_fill_color = scale_fill_sus(rlang::sym(map_var()), "FF"),
            get_line_color = "#FFFFFF", line_width_units = "pixels",
            get_line_width = scale_line_width_sus(select_id()),
            extruded = if (tile() == "auto_zoom" && zoom() == "building") 
              TRUE else FALSE, material = FALSE, get_elevation = 5)
      })
    })
  })
}
