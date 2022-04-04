#### RDECK CHANGE SERVER #######################################################

#' @param map_id Namespace id of the map to redraw, likely to be `NS(id, "map")`
#' @return An updated version of the rdeck map.

rdeck_server <- function(id, map_id, tile, tile2, map_var, zoom, select_id,
                         fill = scale_fill_sus, 
                         fill_args = reactive(list(map_var())),
                         colour = scale_colour_sus, 
                         colour_args = reactive(list(NULL)),
                         lwd = scale_lwd_sus, 
                         lwd_args = reactive(list(select_id())),
                         line_units = "pixels") {
  
  ## Setup ---------------------------------------------------------------------
  
  # Error checking
  stopifnot(is.reactive(tile))
  stopifnot(is.reactive(tile2))
  stopifnot(is.reactive(map_var))
  stopifnot(is.reactive(select_id))
  stopifnot(is.reactive(zoom))
  
  
  ## Module --------------------------------------------------------------------
  
  moduleServer(id, function(input, output, session) {
    
    # Helper variables
    pick <- reactive(
      # Always pickable unless in DA/building
      !tile() %in% c("building", "DA") || 
        # Start at 14.5 for housing-building
        (id == "housing" && zoom() >= 14.5) ||
        # Start at 13.5 for other building layers
        (id != "housing" && zoom() >= 13.5) ||
        # Start at 10.5 for DA
        (tile() == "DA" && zoom() >= 10.5))
    
    extrude <- reactive((tile() == "auto_zoom" && zoom() >= 15.5) | 
                          tile() == "building")
    
    # Create final tileset string
    tile_string <- reactive(paste0(tile(), tile2()))
    
    # Update data layer on variable change
    observeEvent({
      map_var()
      select_id()
      fill_args()
      colour_args()
      lwd_args()
      zoom}, {
      rdeck_proxy(map_id) |>
        add_mvt_layer(
          id = id, 
          pickable = pick(),
          auto_highlight = TRUE, 
          highlight_color = "#FFFFFF50", 
          get_fill_color = do.call(fill, fill_args()),
          get_line_color = do.call(colour, colour_args()),
          get_line_width = do.call(lwd, lwd_args()),
          line_width_units = line_units, 
          extruded = extrude(), 
          material = FALSE,
          get_elevation = 5)
    })
    
    # Update data layer source on tile change
    observeEvent(tile_string(), {
      rdeck_proxy(map_id) |>
        add_mvt_layer(
          id = id, 
          data = mvt_url(paste0("sus-mcgill.", id, "-", tile_string())),
          pickable = pick(), 
          auto_highlight = TRUE, 
          highlight_color = "#FFFFFF50", 
          get_fill_color = do.call(fill, fill_args()),
          get_line_color = do.call(colour, colour_args()),
          get_line_width = do.call(lwd, lwd_args()),
          line_width_units = line_units, 
          extruded = extrude(), 
          material = FALSE, 
          get_elevation = 5)
        
    })
    
  })
}
