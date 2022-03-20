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
                         tile() == "building" && zoom() %in% 
                         c("borough", "CT")))
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
          id = id, 
          pickable = pick(),
          auto_highlight = TRUE, 
          highlight_color = "#FFFFFF50", 
          get_fill_color = scale_fill_sus(rlang::sym(map_var()), "FF"),
          get_line_color = "#FFFFFF", 
          line_width_units = "pixels", 
          get_line_width = scale_line_width_sus(select_id()),
          extruded = extrude(), 
          material = FALSE,
          get_elevation = 5)
    )
    
    # Update layer sources on tile change
    observeEvent(tile(), {
      rdeck_proxy(map_id) |>
        
        # Update data layer
        add_mvt_layer(
          id = id, 
          data = mvt_url(paste0("sus-mcgill.", id, "-", tile())),
          pickable = pick(), 
          auto_highlight = TRUE, 
          highlight_color = "#FFFFFF50", 
          get_fill_color = scale_fill_sus(rlang::sym(map_var()), "FF"),
          get_line_color = "#FFFFFF", 
          line_width_units = "pixels",
          get_line_width = scale_line_width_sus(select_id()),
          extruded = extrude(), 
          material = FALSE, 
          get_elevation = 5) |> 
        
        # Update street layer 1
        add_mvt_layer(
          id = paste0(id, "_street_1"),
          data = if (tile() %in% c("borough", "CT", "DA", "auto_zoom")) 
            mvt_url("sus-mcgill.street_1") else NULL,
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 2,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 15,
          get_line_color = "#FFFFFF",
          get_fill_color = "#A9A9A94D") |> 
        
        # Update street layer 2
        add_mvt_layer(
          id = paste0(id, "_street_2"),
          data = if (tile() %in% c("borough", "CT", "DA", "auto_zoom")) 
            mvt_url("sus-mcgill.street_2") else NULL,
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 1,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 8,
          get_line_color = "#FFFFFF",
          get_fill_color = "#A9A9A94D") |> 
        
        # Update street layer 3
        add_mvt_layer(
          id = paste0(id, "_street_3"),
          data = if (tile() %in% c("borough", "CT", "DA", "auto_zoom")) 
            mvt_url("sus-mcgill.street_3") else NULL,
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 0.5,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 4,
          get_line_color = "#FFFFFF",
          get_fill_color = "#A9A9A94D") |> 
        
        # Update building layer
        add_mvt_layer(
          id = paste0(id, "_building"), 
          data = if (tile() %in% c("borough", "CT", "DA")) mvt_url(
            "sus-mcgill.DA_building_empty") else NULL, 
          pickable = FALSE,
          auto_highlight = FALSE, 
          get_fill_color = "#FFFFFF55", 
          extruded = TRUE, 
          material = FALSE, 
          get_elevation = 5)
      })
    
    # Update street visibility on zoom
    observeEvent(zoom(), {
        rdeck_proxy(map_id) |> 
          add_mvt_layer(
            id = paste0(id, "_street_1"),
            visible = show_street(),
            get_line_width = 15,
            line_width_units = "meters",
            line_width_min_pixels = 2,
            line_joint_rounded = TRUE,
            line_cap_rounded = TRUE,
            get_line_color = "#FFFFFF",
            get_fill_color = "#A9A9A94D") |> 
        # Update street layer 2
        add_mvt_layer(
          id = paste0(id, "_street_2"),
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 1,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 8,
          get_line_color = "#FFFFFF",
          get_fill_color = "#A9A9A94D") |> 
        add_mvt_layer(
          id = paste0(id, "_street_3"),
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 0.5,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 4,
          get_line_color = "#FFFFFF",
          get_fill_color = "#A9A9A94D")
        
    })
  })
}
