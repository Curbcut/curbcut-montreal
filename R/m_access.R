### ACCESS MODULE ##############################################################

# UI ----------------------------------------------------------------------

access_UI <- function(id) {
  id_map <- paste0(id, "-map")

  return(tagList(
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
      select_var_UI(NS(id, id), select_var_id = "d_2",
                    var_list = var_left_list_2_access,
                    label = cc_t(r = r, "Timing")),
      select_var_UI(NS(id, id), select_var_id = "d_1",
                    var_list = var_left_list_1_access,
                    label = cc_t(r = r, "Destination type")),
      slider_UI(NS(id, id), label = cc_t(r = r, "Time threshold"),
                  min = 10, max = 60, step = 1, value = 30)),
      bottom = div(class = "bottom_sidebar",
          tagList(legend_UI(NS(id, id)),
                  hidden(zoom_UI(NS(id, id), map_zoom_levels_CMA))))),
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, id_map), height = "100%")),

    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, id)),
      dyk_UI(NS(id, id)))
  ))
}


# Server ------------------------------------------------------------------

access_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Sidebar
    sidebar_server(id = id, r = r, x = "access")
    
    # Initial reactives
    poi <- reactiveVal(NULL)
    
    # Map
    output[[id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = isolate(r[[id]]$zoom()))) |> 
        add_mvt_layer(id = id) |> 
        add_mvt_layer(id = "metro_lines",
                      data = mvt_url("sus-mcgill.metro_lines"),
                      get_line_color = !!rlang::sym("fill"),
                      get_line_width = 2,
                      line_width_units = "pixels")
    })
    
    # Zoom and POI reactives
    observe({
      r[[id]]$zoom(get_zoom(get_view_state(id_map)$zoom))
      new_poi <- observe_map(get_view_state(id_map))
      if ((is.null(new_poi) && !is.null(poi())) ||
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    }) |> bindEvent(get_view_state(id_map))
    
    # Click reactive
    observe({
      selection <- get_clicked_object(id_map)$ID
      if (!is.na(r[[id]]$select_id()) && 
          selection == r[[id]]$select_id()) {
        r[[id]]$select_id(NA)
      } else r[[id]]$select_id(selection)
    }) |> bindEvent(get_clicked_object(id_map))
    
    # Default location
    observe({
      if (is.null(r$default_select_id())) return(NULL)
      
      new_id <- data()$ID[data()$ID %in% 
                            r$default_select_id()[[gsub("_.*", "", r[[id]]$df())]]]
      if (length(new_id) == 0) return(NULL)
      
      r[[id]]$select_id(new_id)
    }) |> bindEvent(r$default_select_id(), r[[id]]$df())
    
    # Time
    time <- reactive("2016")
    
    # Choose tileset
    tile <- reactive("CMA_CT")

    # Enable or disable slider + type of destination
    observeEvent({r[[id]]$select_id()
      var_right()}, {
        toggle("access-slider", condition = !is.na(r[[id]]$select_id()) && 
                 var_right() == " ")
        toggle("access-d_1", condition = is.na(r[[id]]$select_id()) || 
                 var_right() != " ")
      })

    # Slider widget
    slider <- slider_server(id = id)

    # Manual legend breaks
    breaks <- reactive({
      if (!is.na(r[[id]]$select_id()) && var_right() == " ") {
        breaks <- slider() / 5 * 5:0
        attr(breaks, "label") <- cc_t(r = r, "Minutes to reach census tract")
        attr(breaks, "palette") <- legend_iso
        breaks
      } else NULL
    })
    
    # Left variable servers
    var_left_1 <- select_var_server(id, r = r, select_var_id = "d_1",
                                    var_list = reactive(var_left_list_1_access))
    var_left_2 <- select_var_server(id, r = r, select_var_id = "d_2",
                                    var_list = reactive(var_left_list_2_access))

    # Construct left variable string
    var_left <- reactive(paste0(var_left_1(), "_", var_left_2(), "_count"))
    
    # Compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = make_dropdown(compare = TRUE),
      time = time)

    # If there's a select_id, update the compare to " "
    observeEvent(r[[id]]$select_id(), {
      updatePickerInput(
        session,
        inputId = "access-access-var",
        choices = cc_t(r = r, make_dropdown(compare = TRUE)),
        selected = " ")
    }, priority = 1)

    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      geo = reactive("CMA"),
      var_left = var_left(), 
      var_right = var_right()))
    
    # Explore panel
    explore_content <- explore_server(
      id = id,
      r = r,
      data = data,
      geo = reactive("CMA"),
      var_left = var_left,
      var_right = var_right)

    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      breaks = breaks)
    
    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    data_color <- reactive({
      if (!is.na(r[[id]]$select_id()) && var_right() == " ") {
        tt_thresh <- slider() * 60

        # SQL retrieval
        db_call <- 
          paste0("SELECT timing, destination, `", r[[id]]$select_id(), 
                 "` FROM tt_matrix WHERE timing = '", var_left_2(), "'",
                 " AND `", r[[id]]$select_id(), "` <= ", tt_thresh,
                 " AND destination != ", r[[id]]$select_id())
        CTs_to_map <- do.call("dbGetQuery", list(rlang::sym("tt_matrix_conn"), 
                                                 db_call))
        # Further manipultaion
        CTs_to_map$group <- as.character(6 - ceiling((
          CTs_to_map[[r[[id]]$select_id()]]) / tt_thresh * 5))
        CTs_to_map <- CTs_to_map[, c("destination", "group")]
        CTs_to_map <- merge(CTs_to_map, colour_iso, by = "group")
        names(CTs_to_map) <- c("group", "ID", "fill")
        data_1 <- data()[, "ID"] |> merge(CTs_to_map, by = "ID")
        data_1 <- data_1[, c("ID", "fill")]
        data_2 <- data()[data()$ID == r[[id]]$select_id(), "ID"]
        data_2$fill <- "#000000"
        out <- rbind(data_1, data_2)
        names(out) <- c("group", "value")
        out
      } else {
        # Data color
        get_data_color(
          map_zoom_levels = rlang::set_names("CT", "CT"),
          geo = "CMA",
          var_left = var_left(), 
          var_right = var_right())
      }
    })

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      r = r,
      map_id = "map",
      data_color = data_color,
      tile = tile)
    
    # Update map labels
    label_server(
      id = id, 
      r = r,
      map_id = "map", 
      tile = tile)
    
    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      df = r[[id]]$df,
      map_viewstate = reactive(get_view_state(paste0(id, "-map"))),
      var_left = var_left,
      var_right = var_right,
      more_args = reactive(c("s-slider" = slider()))
    )
    
    # Data transparency and export
    r[[id]]$export_data <- reactive(data_export(id = id,
                                                data = data(),
                                                var_left = var_left(),
                                                var_right = var_right(),
                                                df = r[[id]]$df()))

  })
}
