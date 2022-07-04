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
                    label = sus_translate(r = r, "250-metre grid"))),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, id)),
                           zoom_UI(NS(id, id), map_zoom_levels)))),
    
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
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    poi <- reactiveVal(NULL)

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
    
    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[id]]$zoom(), map_zoom_levels)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[id]]$zoom())

    # Click reactive
    observe({
      selection <- get_clicked_object(id_map)$ID
      if (!is.na(r[[id]]$select_id()) && 
          selection == r[[id]]$select_id()) {
        r[[id]]$select_id(NA)
      } else r[[id]]$select_id(selection)
    }) |> bindEvent(get_clicked_object(id_map))
    
    # Grid value
    grid <- checkbox_server(id = id)
    
    # Zoom level for data
    tile_choropleth <- zoom_server(
      id = id, 
      r = r,
      zoom_string = zoom_string, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Choose tileset
    tile <- reactive(if (grid()) "grid" else tile_choropleth())
    
    # Get df for explore/legend/etc
    observe(r[[id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string(), ignoreInit = TRUE)
    
    # Time
    time <- reactive("2016")
    
    # Left variable server
    var_left <- select_var_server(id, r = r, var_list = reactive(
      make_dropdown(only = list(theme = "Climate risk"))))
    
    # observe(print(var_right()))
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = id, 
      r = r,
      var_list = make_dropdown(compare = TRUE),
      time = time)
    
    # Additional tileset identifier
    tile2 <- reactive({
      tile_lookup$suffix[tile_lookup$module == "climate_risk" &
                           tile_lookup$tile2 == var_left()]
    })
    
    # Composite variable for map
    map_var <- reactive(
      str_remove(paste(var_left(), var_right(), sep = "_"), "_ $"))
    
    # Sidebar
    sidebar_server(id = id, r = r, x = "climate_risk")
    
    # Data
    data <- reactive(get_data(
      df = r[[id]]$df(),
      var_left = var_left(), 
      var_right = var_right(), 
      island = TRUE))
    
    # Legend
    legend <- legend_server(
      id = id,
      r = r,
      data = data,
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
      tile2 = tile2,
      map_var = map_var, 
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
      var_right = var_right,
      more_args = reactive(c("c-cbox" = as.logical(grid())))
    )
    
    # Data transparency and export
    observe({
      r[[id]]$export_data(data_export(id = id, 
                                      data = data(), 
                                      var_left = var_left(), 
                                      var_right = var_right(), 
                                      df = df()))
    })
    
  })
}
