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
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    
    # Map
    output[[ns_id_map]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom))
    })
    
    # Zoom and POI reactives
    observeEvent(get_view_state(ns_id_map), {
      zoom({
        if (!is.null(r$sus_bookmark$zoom)) {
          r$sus_bookmark$zoom
        } else if (!is.null(r$sus_link$zoom)) {
          r$sus_link$zoom
        } else get_zoom(get_view_state(ns_id_map)$zoom)})
      new_poi <- observe_map(get_view_state(ns_id_map))
      if ((is.null(new_poi) && !is.null(poi())) || 
          (!is.null(new_poi) && (is.null(poi()) || !all(new_poi == poi()))))
        poi(new_poi)
    })
    
    # Zoom string reactive
    observeEvent(zoom(), {
      new_zoom_string <- get_zoom_string(zoom(), map_zoom_levels)
      if (new_zoom_string != zoom_string()) zoom_string(new_zoom_string)
    })
    
    # Click reactive
    observeEvent(get_clicked_object(ns_id_map), {
      selection <- get_clicked_object(ns_id_map)$ID
      if (!is.na(select_id()) && selection == select_id()) return(select_id(NA))
      
      select_id(selection)
    })
    
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
    df <- reactive(get_df(tile(), zoom_string(), r = r))
    
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
      df = df(), 
      var_left = var_left(), 
      var_right = var_right(), 
      island = TRUE))
    
    # Legend
    legend <- legend_server(
      id = ns_id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df)

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
      map_id = "map", 
      tile = tile, 
      tile2 = tile2,
      map_var = map_var, 
      zoom = zoom,
      select_id = select_id,
      lwd = scale_lwd_climate_risk, 
      lwd_args = reactive(list(select_id(), tile())))
    
    # Update map labels
    label_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      zoom = zoom)
    
    # De-select
    observeEvent(input[[paste0(ns_id, "-clear_selection")]], select_id(NA))
    # Error check
    observeEvent(data(), if (!select_id() %in% data()$ID) select_id(NA),
                 ignoreInit = TRUE)
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      select_id = select_id)
    
    # If grid isn't clicked, toggle on the zoom menu
    observeEvent(grid(), {
      toggle("climate_risk-zoom_auto", condition = !grid())
      toggle("climate_risk-zoom_slider", condition = !grid())
    })
    
    # Bookmarking
    bookmark_server(
      id = ns_id,
      r = r,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = "map",
      more_args = reactive(c("c-cbox" = as.logical(grid())))
    )
    
    # Update select_id() on bookmark
    observeEvent(r$sus_bookmark$active, {
      if (isTRUE(r$sus_bookmark$active)) {
        delay(1000, {
          if (!is.null(r$sus_bookmark$select_id))
            if (r$sus_bookmark$select_id != "NA") 
              select_id(r$sus_bookmark$select_id)
        })
      }
      # So that bookmarking gets triggered only ONCE
      delay(1500, {
        r$sus_bookmark$active <- FALSE
        r$sus_bookmark$df <- NULL
        r$sus_bookmark$zoom <- NULL
      })
    }, priority = -2)
    
    # Update select_id() on module link
    observeEvent(r$sus_link$activity, {
      delay(1000, {
        if (!is.null(r$sus_link$select_id)) select_id(r$sus_link$select_id)
        r$sus_link$df <- NULL
        r$sus_link$zoom <- NULL
      })
    }, priority = -2)
    
    # Return for data transprency and export
    export_data <- reactive(data_export(id = id, 
                                        data = data(), 
                                        var_left = var_left(), 
                                        var_right = var_right(), 
                                        df = df()))
    
    observe({assign("data", data(), pos = 1)})
    observe({assign("df", df(), pos = 1)})
    observe({assign("var_left", var_left(), pos = 1)})
    observe({assign("var_right", var_right(), pos = 1)})
    
    
    observe({assign("export_data_", export_data(), pos = 1)})
    
    return(export_data)
    
  })
}
