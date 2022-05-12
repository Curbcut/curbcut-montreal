### CLIMATE RISK MODULE ########################################################

# UI ----------------------------------------------------------------------

climate_risk_UI <- function(id) {
  ns_id <- "climate_risk"
  ns_id_map <- paste0(ns_id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        select_var_UI(NS(id, ns_id), var_list = make_dropdown(
          only = list(theme = "Climate risk"))), 
        checkbox_UI(NS(id, ns_id), value = TRUE,
                    label = sus_translate(r = r, "250-metre grid"))),
      bottom = div(class = "bottom_sidebar",
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, ns_id_map), height = "100%")),
    
    # Right panel
    right_panel(
      id = id, 
      compare_UI(NS(id, ns_id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, ns_id)), 
      dyk_UI(NS(id, ns_id)))
    
  )
}


# Server ------------------------------------------------------------------

climate_risk_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "climate_risk"
    ns_id_map <- paste0(ns_id, "-map")
    
    # Initial reactives
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    poi <- reactiveVal(NULL)

    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = isolate(r[[ns_id]]$zoom())))
    })
    
    # Zoom and POI reactives
    observe({
      r[[ns_id]]$zoom(get_zoom(get_view_state(ns_id_map)$zoom))
      new_poi <- observe_map(get_view_state(ns_id_map))
      if ((is.null(new_poi) && !is.null(poi())) ||
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    }) |> bindEvent(get_view_state(ns_id_map))
    
    # Zoom string reactive
    observe({
      new_zoom_string <- get_zoom_string(r[[ns_id]]$zoom(), map_zoom_levels)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    }) |> bindEvent(r[[ns_id]]$zoom())

    # Click reactive
    observe({
      selection <- get_clicked_object(ns_id_map)$ID
      if (!is.na(r[[ns_id]]$select_id()) && 
          selection == r[[ns_id]]$select_id()) {
        r[[ns_id]]$select_id(NA)
      } else r[[ns_id]]$select_id(selection)
    }) |> bindEvent(get_clicked_object(ns_id_map))
    
    # Grid value
    grid <- checkbox_server(id = ns_id)
    
    # Zoom level for data
    tile_choropleth <- zoom_server(
      id = ns_id, 
      r = r,
      zoom_string = zoom_string, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Choose tileset
    tile <- reactive(if (grid()) "grid" else tile_choropleth())
    
    # Get df for explore/legend/etc
    observe(r[[ns_id]]$df(get_df(tile(), zoom_string()))) |> 
      bindEvent(tile(), zoom_string(), ignoreInit = TRUE)
    
    # Time
    time <- reactive("2016")
    
    # Left variable server
    var_left <- select_var_server(ns_id, r = r, var_list = reactive(
      make_dropdown(only = list(theme = "Climate risk"))))
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = ns_id, 
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
    sidebar_server(id = ns_id, r = r, x = "climate_risk")
    
    # Data
    data <- reactive(get_data(
      df = r[[ns_id]]$df(),
      var_left = var_left(), 
      var_right = var_right(), 
      island = TRUE))
    
    # Legend
    legend <- legend_server(
      id = ns_id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right)

    # Did-you-know panel
    dyk_server(
      id = ns_id,
      r = r,
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id, 
      r = r, 
      map_id = "map",
      tile = tile, 
      tile2 = tile2,
      map_var = map_var, 
      lwd = scale_lwd_climate_risk, 
      lwd_args = reactive(list(r[[ns_id]]$select_id(), tile())))
    
    # Update map labels
    label_server(
      id = ns_id, 
      r = r,
      map_id = "map", 
      tile = tile)

    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
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
      id = ns_id,
      r = r,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      var_right = var_right,
      more_args = reactive(c("c-cbox" = as.logical(grid())))
    )
    
  })
}
