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
    
    # Helper variables
    pick <- reactive(!((tile() == "DA" && zoom() == "borough") ||
                         tile() == "building" && zoom() %in% c("borough", "CT")))
    extrude <- reactive((tile() == "auto_zoom" && zoom() == "building") | 
                          tile() == "building")
    show_street <- reactive(tile() %in% c("borough", "CT", "DA") ||
                              (tile() == "auto_zoom" && zoom() != "building"))
    
    # Update data layer on variable change or selection 
    observeEvent({
      map_var()
      select_id()
      }, rdeck_proxy(map_id) |>
        add_mvt_layer(
          id = id, auto_highlight = TRUE, highlight_color = "#FFFFFF80", 
          pickable = pick(),
          get_fill_color = scale_fill_sus(rlang::sym(map_var()), "FF"),
          get_line_color = "#FFFFFF", line_width_units = "pixels", 
          get_line_width = scale_line_width_sus(select_id()),
          extruded = extrude(), material = FALSE, get_elevation = 5)
    )
    
    # Update layer sources on tile change
    observeEvent(tile(), {
      observe({
        rdeck_proxy(map_id) |>
          # Update data layer
          add_mvt_layer(
            id = id, data = mvt_url(paste0("sus-mcgill.", id, "-", tile())),
            auto_highlight = TRUE, highlight_color = "#FFFFFF80", 
            pickable = pick(),
            get_fill_color = scale_fill_sus(rlang::sym(map_var()), "FF"),
            get_line_color = "#FFFFFF", line_width_units = "pixels",
            get_line_width = scale_line_width_sus(select_id()),
            extruded = extrude(), material = FALSE, get_elevation = 5) |> 
          # Update building layer
          add_mvt_layer(
            id = paste0(id, "_building"), 
            data = if (tile() %in% c("borough", "CT", "DA")) mvt_url(
              "sus-mcgill.building_add")) else NULL, pickable = FALSE,
            auto_highlight = FALSE, get_fill_color = "#FFFFFF55", 
            extruded = TRUE, material = FALSE, get_elevation = 5)
      })
    })
    
    # Update street/building layers on zoom or tile change
    observeEvent({
      tile()
      zoom()}, {
        rdeck_proxy(map_id) |> 
          add_mvt_layer(
            id = paste0(id, "_street"),
            line_width_units = "pixels",
            get_line_width = scale_line_width_street_sus(),
            get_line_color = if (show_street()) "#FFFFFFFF" else "#FFFFFF00",
            get_fill_color = if (show_street()) "#A9A9A94D" else "#A9A9A900")
    })
  })
}
