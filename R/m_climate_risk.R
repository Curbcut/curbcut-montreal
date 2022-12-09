### CLIMATE RISK MODULE ########################################################

# UI ----------------------------------------------------------------------

climate_risk_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        select_var_UI(NS(id, id), var_list = make_dropdown(
          only = list(theme = "Climate risk"))), 
        checkbox_UI(NS(id, id), value = TRUE,
                    label = cc_t(r = r, "250-metre grid"))),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, id)),
                           zoom_UI(NS(id, id), map_zoom_levels_CMA)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id, 
      compare_UI(NS(id, id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, id)), 
      dyk_UI(NS(id, id)))
    
  )
}


# Server ------------------------------------------------------------------

climate_risk_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    # Initial reactives
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels_island,
                                               "CSD"))
    poi <- reactiveVal(NULL)
    tweaked_geo <- reactive(if (r$geo() == "CMA") "island" else r$geo())

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
    
    # Map zoom levels change depending on r$geo()
    map_zoom_levels <- eventReactive(tweaked_geo(), {
      get_zoom_levels(default = "island", 
                      geo = tweaked_geo(),
                      var_left = var_left())
    })
    
    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels()$levels,
                                         map_zoom_levels()$region)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[id]]$zoom(), map_zoom_levels()$levels, map_zoom_levels()$region)

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
    
    # Grid value
    grid <- checkbox_server(id = id)
    
    # Zoom level for data
    tile_choropleth <- zoom_server(
      id = id,
      r = r,
      zoom_string = zoom_string,
      zoom_levels = map_zoom_levels)
    
    # Choose tileset
    tile <- reactive({if (grid()) "grid_grid" else tile_choropleth()})
    
    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string())

    # Time
    time <- reactive("2016")
    
    # Left variable server
    var_left <- 
      select_var_server(id, 
                        r = r, 
                        var_list = reactive(
                          make_dropdown(only = list(theme = "Climate risk"))))
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = id, 
      r = r,
      var_list = make_dropdown(compare = TRUE),
      time = time)
    
    # Sidebar
    sidebar_server(id = id, r = r, x = "climate_risk")
    
    # Update region depending on if grid is selected
    region <- reactive(if (grid()) "grid" else map_zoom_levels()$region)
    
    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      geo = region(),
      var_left = var_left(),
      var_right = var_right()))

    # Data for tile coloring
    data_color <- reactive(get_data_color(
      map_zoom_levels = if (grid()) rlang::set_names("grid", "grid") else
        map_zoom_levels()$levels,
      geo = region(),
      var_left = var_left(),
      var_right = var_right()
    ))

    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      data = data,
      geo = region,
      var_left = var_left,
      var_right = var_right)

    # Did-you-know panel
    dyk_server(
      id = id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)

    # Update map in response to variable changes or zooming
    rdeck_server(
      id = id,
      r = r,
      map_id = "map",
      tile = tile,
      data_color = data_color,
      lwd = scale_lwd_climate_risk,
      lwd_args = reactive(list(r[[id]]$select_id(), tile())))

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
      geo = region,
      var_left = var_left,
      var_right = var_right)

    # If grid isn't clicked, toggle on the zoom menu
    observe({
      toggle("climate_risk-zoom_auto", condition = !grid())
      toggle("climate_risk-zoom_slider", condition = !grid())
    }) |> bindEvent(grid())

    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id,
      df = r[[id]]$df,
      map_viewstate = reactive(get_view_state(id_map)),
      var_left = var_left,
      var_right = var_right,
      more_args = reactive(c("c-cbox" = as.logical(grid())))
    )
    
    observe({
      assign("data", data(), envir = .GlobalEnv)
      assign("df", r[[id]]$df(), envir = .GlobalEnv)
      assign("var_right", var_right(), envir = .GlobalEnv)
      assign("var_left", var_left(), envir = .GlobalEnv)
      assign("geo", region(), envir = .GlobalEnv)
    })
    
    # Data transparency and export
    r[[id]]$export_data <- reactive(data_export(id = id,
                                                data = data(),
                                                var_left = var_left(),
                                                var_right = var_right(),
                                                df = r[[id]]$df()))
    
  })
}
