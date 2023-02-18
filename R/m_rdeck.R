#### RDECK CHANGE SERVER #######################################################

#' @param map_id Namespace id of the map to redraw, likely to be `NS(id, "map")`
#' @return An updated version of the rdeck map.
#' 
rdeck_UI <- function(id) {
  div(class = "map_div", rdeckOutput(NS(id, "map"), height = "100%"))
}

rdeck_server <- function(id, r, tile, data_color,
                         select_id = r[[id]]$select_id, zoom_levels,
                         zoom = r[[id]]$zoom,
                         fill = scale_fill_sus, 
                         fill_args = reactive(list(data_color())),
                         colour = scale_colour_sus, 
                         colour_args = reactive(list(NULL)),
                         lwd = scale_lwd_sus, 
                         lwd_args = reactive(list(select_id(), tile(), zoom(),
                                                  zoom_levels())),
                         line_units = "pixels") {
  
  ## Module --------------------------------------------------------------------
  
  moduleServer(id, function(input, output, session) {
    
    # Map
    output$map <- renderRdeck({
      rdeck(
        map_style = map_base_style, layer_selector = FALSE,
        initial_view_state = view_state(
          center = map_loc, zoom = isolate(r[[id]]$zoom())
        )
      )
    })
    
    # Helper variables
    extrude <- reactive((grepl("auto_zoom$", tile()) && zoom() >= 15.5) | 
                          grepl("building", tile()))
    highlight <- reactive(if (id == "natural_inf") FALSE else TRUE)
    
    # Update data layer source on tile change
    observe(
      rdeck_proxy("map") |>
        add_mvt_layer(
          id = id, 
          data = mvt_url(paste0(mapbox_username, ".", tileset_prefix, "_", tile())),
          pickable = TRUE, 
          auto_highlight = highlight(), 
          highlight_color = "#FFFFFF50", 
          get_fill_color = do.call(fill, fill_args()),
          get_line_color = do.call(colour, colour_args()),
          get_line_width = do.call(lwd, lwd_args()),
          line_width_units = line_units, 
          extruded = extrude(), 
          material = FALSE, 
          get_elevation = 5)) |> bindEvent(tile())
    
    # Update data layer on variable change
    observe(
      rdeck_proxy("map") |>
        add_mvt_layer(
          id = id, 
          pickable = TRUE,
          auto_highlight = highlight(), 
          highlight_color = "#FFFFFF50", 
          get_fill_color = do.call(fill, fill_args()),
          get_line_color = do.call(colour, colour_args()),
          get_line_width = do.call(lwd, lwd_args()),
          line_width_units = line_units, 
          extruded = extrude(), 
          material = FALSE,
          get_elevation = 5))
    
  })
}
