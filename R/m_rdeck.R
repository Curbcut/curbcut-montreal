#### RDECK CHANGE SERVER #######################################################

#' @param map_id Namespace id of the map to redraw, likely to be `NS(id, "map")`
#' @return An updated version of the rdeck map.

rdeck_server <- function(id, map_id, tile, tile2, map_var, zoom, select_id) {
  
  ## Setup ---------------------------------------------------------------------
  
  # Error checking
  stopifnot(is.reactive(tile))
  stopifnot(is.reactive(tile2))
  stopifnot(is.reactive(map_var))
  stopifnot(is.reactive(select_id))
  stopifnot(is.reactive(zoom))
  
  moduleServer(id, function(input, output, session) {
    
    # Helper variables
    pick <- reactive(
      # Always pickable unless in DA/building
      !tile() %in% c("building", "DA") || 
        # Start at 14.5 for building-housing
        (id == "housing" && zoom() >= 14.5) ||
        # Start at 13.5 for other building layers
        (id != "housing" && zoom() >= 13.5) ||
        # Start at 10.5 for DA
        (tile() == "DA" && zoom() >= 10.5))
    extrude <- reactive((tile() == "auto_zoom" && zoom() >= 15.5) | 
                          tile() == "building")
    show_street <- reactive(tile() %in% c("borough", "CT", "DA", "grid") ||
                              (tile() == "auto_zoom" && zoom() < 15.5))
    show_label <- reactive(tile() %in% c("borough") || 
                             (tile() != "building" && zoom() < 12.5))
    
    # Create final tileset string
    tile_string <- reactive(paste0(tile(), tile2()))
    
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
          get_fill_color = if (id == "alley" && tile() == "empty_borough") 
            "#FFFFFF00" else scale_fill_sus(rlang::sym(map_var())),
          get_line_color = if (id == "alley") 
            scale_line_color_alley(map_var(), tile()) else "#FFFFFF", 
          line_width_units = "pixels", 
          get_line_width = scale_line_width_sus(id, select_id(), tile()),
          extruded = extrude(), 
          material = FALSE,
          get_elevation = 5)
    )
    
    # Update layer sources on tile change
    observeEvent(tile_string(), {
      rdeck_proxy(map_id) |>
        
        # Update data layer
        add_mvt_layer(
          id = id, 
          data = mvt_url(paste0("sus-mcgill.", id, "-", tile_string())),
          pickable = pick(), 
          auto_highlight = TRUE, 
          highlight_color = "#FFFFFF50", 
          get_fill_color = if (id == "alley" && tile() == "empty_borough") 
            "#FFFFFF00" else scale_fill_sus(rlang::sym(map_var())),
          get_line_color = if (id == "alley") 
            scale_line_color_alley(map_var(), tile()) else "#FFFFFF", 
          line_width_units = "pixels",
          get_line_width = scale_line_width_sus(id, select_id(), tile()),
          extruded = extrude(), 
          material = FALSE, 
          get_elevation = 5) |> 
        
        # Update street layer 1
        add_mvt_layer(
          id = paste0(id, "_street_1"),
          data = if (tile() %in% c("borough", "CT", "DA", "grid", "auto_zoom")) 
            mvt_url("sus-mcgill.street_1") else "",
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 2,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 15,
          get_line_color = "#FFFFFFBB",
          get_fill_color = "#A9A9A94D") |> 
        
        # Update street layer 2
        add_mvt_layer(
          id = paste0(id, "_street_2"),
          data = if (tile() %in% c("borough", "CT", "DA", "grid", "auto_zoom")) 
            mvt_url("sus-mcgill.street_2") else "",
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 1,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 8,
          get_line_color = "#FFFFFFBB",
          get_fill_color = "#A9A9A94D") |> 
        
        # Update street layer 3
        add_mvt_layer(
          id = paste0(id, "_street_3"),
          data = if (tile() %in% c("borough", "CT", "DA", "grid", "auto_zoom")) 
            mvt_url("sus-mcgill.street_3") else "",
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 0.5,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 4,
          get_line_color = "#FFFFFFBB",
          get_fill_color = "#A9A9A94D") |> 
        
        # Update building layer
        add_mvt_layer(
          id = paste0(id, "_building"),
          data = if (tile() %in% c("borough", "CT", "DA", "grid"))
            mvt_url("sus-mcgill.DA_building_empty") else "",
          pickable = FALSE,
          auto_highlight = FALSE,
          get_fill_color = "#FFFFFF55",
          extruded = TRUE,
          material = FALSE,
          get_elevation = 5) |>
        
        # Update label layer
        add_mvt_layer(
          id = paste0(id, "_borough_labels"), 
          data = if (tile() %in% c("borough", "CT", "grid", "auto_zoom")) 
            mvt_url("sus-mcgill.borough_label") else "",
          visible = show_label(),
          point_type = "text", get_text = rlang::sym("name"),
          text_background = TRUE,
          text_background_padding = rep(2, 4),
          text_font_family = "source-sans-pro-regular",
          text_font_weight = "bold",
          get_text_color = "#000000FF",
          get_text_size = 10,
          get_text_background_color = "#FFFFFF90",
          get_text_border_color = "#00000000",
          get_text_border_width = 0
        )
    })
    
    # Update data pickability and street/label visibility on zoom
    observeEvent(zoom(), {
        rdeck_proxy(map_id) |> 
        
        # Update data layer
        add_mvt_layer(
          id = id, 
          pickable = pick(), 
          auto_highlight = TRUE, 
          highlight_color = "#FFFFFF50", 
          get_fill_color = if (id == "alley" && tile() == "empty_borough") 
            "#FFFFFF00" else scale_fill_sus(rlang::sym(map_var())),
          get_line_color = if (id == "alley") 
            scale_line_color_alley(map_var(), tile()) else "#FFFFFF", 
          line_width_units = "pixels",
          get_line_width = scale_line_width_sus(id, select_id(), tile()),
          extruded = extrude(), 
          material = FALSE, 
          get_elevation = 5) |> 
        
        # Update street layer 1
        add_mvt_layer(
          id = paste0(id, "_street_1"),
          visible = show_street(),
          get_line_width = 15,
          line_width_units = "meters",
          line_width_min_pixels = 2,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_color = "#FFFFFFBB",
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
          get_line_color = "#FFFFFFBB",
          get_fill_color = "#A9A9A94D") |> 
        
        # Update street layer 3
        add_mvt_layer(
          id = paste0(id, "_street_3"),
          visible = show_street(),
          line_width_units = "meters",
          line_width_min_pixels = 0.5,
          line_joint_rounded = TRUE,
          line_cap_rounded = TRUE,
          get_line_width = 4,
          get_line_color = "#FFFFFFBB",
          get_fill_color = "#A9A9A94D") |>
        
        # Update label layer
        add_mvt_layer(
          id = paste0(id, "_borough_labels"), 
          visible = show_label(),
          point_type = "text", get_text = rlang::sym("name"),
          text_background = TRUE,
          text_background_padding = rep(2, 4),
          text_font_family = "source-sans-pro-regular",
          text_font_weight = "bold",
          get_text_color = "#000000FF",
          get_text_size = 10,
          get_text_background_color = "#FFFFFF90",
          get_text_border_color = "#00000000",
          get_text_border_width = 0
        )
        
    })
  })
}
