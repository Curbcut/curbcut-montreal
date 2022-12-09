#### DWELLING TYPES MODULE #####################################################

# UI ----------------------------------------------------------------------

dw_types_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        select_var_UI(NS(id, id), select_var_id = "d_3",
                      label = cc_t(r = r, "Dwelling type"),
                      var_list = var_left_list_3_dw_types), 
        select_var_UI(NS(id, id), select_var_id = "d_1",
                      label = cc_t(r = r, "Tenure status"),
                      var_list = var_left_list_1_dw_types), 
        select_var_UI(NS(id, id), select_var_id = "d_2",
                      label = cc_t(r = r, "Shelter cost"),
                      var_list = var_left_list_2_dw_types), 
        checkbox_UI(NS(id, id),
                    label = cc_t(r = r, 
                                 "Normalized data (percent of dwellings)"))),
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, id)),
                           zoom_UI(NS(id, id), map_zoom_levels_CMA_max_CT)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, id), cent_compare),
      explore_UI(NS(id, id)),
      dyk_UI(NS(id, id)))
  )
}


# Server ------------------------------------------------------------------

dw_types_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels_CMA_max_CT))
    poi <- reactiveVal(NULL)
    new_poi <- reactiveVal(NULL)
    
    # Map
    output[[id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = isolate(r[[id]]$zoom())))
    })
    
    # Zoom and POI reactives
    observe({
      r[[id]]$zoom(get_zoom(get_view_state(id_map)$zoom))
      new_poi <- observe_map(get_view_state(id_map))
      if ((is.null(new_poi) && !is.null(poi())) ||
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    }) |> bindEvent(get_view_state(id_map))
    
    # Map zoom levels change depending on r$geo(). Listening only to the latter
    # to not have to recalculate everytime var_left() changes.
    map_zoom_levels <- eventReactive(r$geo(), {
      get_zoom_levels(default = "CMA", 
                      geo = r$geo(),
                      var_left = isolate(var_left()),
                      suffix_zoom_levels = "_max_CT")
    })
    
    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels()$levels,
                                         map_zoom_levels()$region)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[id]]$zoom(), map_zoom_levels()$region)
    
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
    
    # Sidebar
    sidebar_server(id = id, r = r)
    # Centraide logo
    observe({
      insertUI(selector = paste0("#", paste(id, id, "title", sep = "-")),
               where = "beforeEnd",
               tags$a(href = "https://www.centraide-mtl.org/", target = "_blank",
                      img(src = paste0("centraide_logo/centraide_logo_", r$lang(), ".png"), 
                          style = 'width:70%; display:block; margin:auto; margin-top:15px; margin-bottom:15px;')))
    })
    
    # Choose tileset
    tile_1 <- zoom_server(
      id = id,
      r = r,
      zoom_string = zoom_string,
      zoom_levels = map_zoom_levels)
    
    tile <- reactive({
      if (!grepl("auto_zoom", tile_1())) return(tile_1())
      paste0(tile_1(), "_max_CT")
    })
    
    # Time
    time <- reactive("2016")
    
    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string())
    
    # Checkbox value
    as_pct <- checkbox_server(id = id)
    
    # Remember if the user wanted normalized data
    onclick("dw_types-cbox", expr = r[[id]]$prev_norm(!r[[id]]$prev_norm()))
    
    # Disable the normalized checkbox if variables is percentage of total
    observeEvent(var_left(), {
      is_total_count <- var_left() %in%
        paste("cent_d_total_total_total", c("count", "pct"), time(), sep = "_")
      if (is_total_count) updateCheckboxInput(inputId = "dw_types-cbox",
                                              value = FALSE)
      
      toggleState("dw_types-cbox", condition = !is_total_count)
      
      if (!is_total_count && r[[id]]$prev_norm())
        updateCheckboxInput(inputId = "dw_types-cbox",
                            value = TRUE)
    })

    # Left variable server
    vl_tn <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_1",
      var_list = reactive(var_left_list_1_dw_types))
    
    vl_sc <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_2",
      var_list = reactive(var_left_list_2_dw_types))
    
    vl_dw <- select_var_server(
      id = id,
      r = r,
      select_var_id = "d_3",
      var_list = reactive(var_left_list_3_dw_types))
    
    # Final left variable server creation
    var_left <- reactive({
      # Resort to _count instead of _pct if all is total
      if (all(c(vl_tn(), vl_sc(), vl_dw()) == "total"))
        return(paste("cent_d_total_total_total_count", time(), sep = "_"))
      
      paste("cent_d",
            vl_tn(), vl_sc(), vl_dw(), 
            if (as_pct()) "pct" else "count",
            time(), sep = "_")
    })
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = id,
      r = r,
      var_list = cent_compare,
      time = time)
    
    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      geo = map_zoom_levels()$region,
      var_left = var_left(),
      var_right = var_right()))
    
    # Data for tile coloring
    data_color <- reactive(get_data_color(
      map_zoom_levels = map_zoom_levels()$levels,
      geo = map_zoom_levels()$region,
      var_left = var_left(),
      var_right = var_right()
    ))

    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right)
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      r = r,
      map_id = "map",
      tile = tile,
      data_color = data_color,
      zoom_levels = reactive(map_zoom_levels()$levels))

    # Update map labels
    label_server(
      id = id,
      r = r,
      map_id = "map",
      tile = tile)
    
    # Explore panel
    explore_content <- explore_server(
      id = id,
      r = r,
      data = data,
      geo = reactive(map_zoom_levels()$region),
      var_left = var_left,
      var_right = var_right)
    
    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      df = r[[id]]$df,
      map_viewstate = reactive(get_view_state(paste0(id, "-map"))),
      var_left = var_left,
      var_right = var_right,
      more_args = reactive(c(
        "c-cbox" = str_extract(as_pct(), "^."),
        "o-p_n" = str_extract(r[[id]]$prev_norm(), "^.")))
    )
    
    # Data transparency and export
    r[[id]]$export_data <- reactive(data_export(id = id,
                                                data = data(),
                                                var_left = var_left(),
                                                var_right = var_right(),
                                                df = r[[id]]$df()))
    
  })
}
