#### RDECK LABEL SERVER ########################################################

label_server <- function(id, map_id, tile, zoom) {
  
  ## Setup ---------------------------------------------------------------------
  
  # Error checking
  stopifnot(is.reactive(tile))
  stopifnot(is.reactive(zoom))
  
  
  ## Module --------------------------------------------------------------------
  
  moduleServer(id, function(input, output, session) {
    
    # Helper variables
    show_street <- reactive(tile() %in% c("borough", "CT", "DA", "grid") ||
                              (tile() == "auto_zoom" && zoom() < 15.5))
    
    show_label <- reactive(
      tile() %in% c("borough") || 
        (tile() %in% c("auto_zoom", "CT", "DA", "grid") && zoom() < 12.5))
    
    # Update label layer sources on tile change
    observeEvent(tile(), {
      rdeck_proxy(map_id) |>
        
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
          point_type = "text", 
          get_text = name,
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
    
    # Update street visibility on zoom
    observeEvent(show_street(), {
      rdeck_proxy(map_id) |>

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
          get_fill_color = "#A9A9A94D")
    })

    # Update label visibility on zoom
    observeEvent(show_label(), {
      rdeck_proxy(map_id) |>
        add_mvt_layer(
          id = paste0(id, "_borough_labels"),
          visible = show_label(),
          point_type = "text", 
          get_text = name,
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
