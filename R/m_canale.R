### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  ns_id <- "canale"
  ns_id_map <- paste0(ns_id, "-map")
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, ns_id), 
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

canale_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "canale"
    ns_id_map <- paste0(ns_id, "-map")

    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    zoom_string <- reactiveVal(get_zoom_string(map_zoom, map_zoom_levels))
    r[[ns_id]]$select_id <- reactive(NA)
    poi <- reactiveVal(NULL)
    r[[ns_id]]$df <- reactive("borough")

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
      if (!is.na(r[[ns_id]]$select_id()) && selection == r[[ns_id]]$select_id()) {
        r[[ns_id]]$select_id <- reactive(NA)
      } else r[[ns_id]]$select_id <- reactive(selection)
    })# |> bindEvent(get_clicked_object(ns_id_map))

    # Choose tileset
    tile <- zoom_server(
      id = ns_id,
      r = r,
      zoom_string = zoom_string,
      zoom_levels = reactive(map_zoom_levels))

    # Get df for explore/legend/etc
    r[[ns_id]]$df <- reactive(get_df(tile(), zoom_string()))
    
    # Time
    time <- reactive("2016")

    # Left variable
    var_left <- reactive(paste0("canale_ind_", time()))

    # Right variable / compare panel
    var_right <- compare_server(
      id = ns_id,
      r = r,
      var_list = make_dropdown(compare = TRUE),
      time = time)

    # Additional tileset identifier
    tile2 <- reactive("")

    # Composite variable for map
    map_var <- reactive(
      str_remove(paste(var_left(), var_right(), sep = "_"), "_ $"))

    # Sidebar
    sidebar_server(id = ns_id, r = r, x = "canale")

    # Data
    data <- reactive(get_data(
      df = r[[ns_id]]$df(),
      var_left = var_left(),
      var_right = var_right()))

    # Legend
    legend <- legend_server(
      id = ns_id,
      r = r,
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
      tile2 =  tile2,
      map_var = map_var,
      zoom = zoom)

    # Update map labels
    label_server(
      id = ns_id,
      map_id = "map",
      tile = tile,
      zoom = zoom)

    # De-select
    observeEvent(input[[paste0(ns_id, "-clear_selection")]],
                 r[[ns_id]]$select_id <- reactive(NA))
    # Error check
    # observeEvent(r[[ns_id]]$df(),
    #              {r[[ns_id]]$select_id <- reactive(NA)},
    #              ignoreInit = TRUE)
    
    # observeEvent(data(), {
    #   if (!r[[ns_id]]$select_id() %in% data()$ID)
    #     r[[ns_id]]$select_id <- reactive(NA)
    #   }, ignoreInit = TRUE)
    
    observe(print(r[[ns_id]]$df()))
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      r = r,
      data = data,
      var_left = var_left,
      var_right = var_right)

    # Bookmarking
    bookmark_server(
      id = ns_id,
      r = r,
      map_viewstate = reactive(get_view_state(ns_id_map)),
      var_right = var_right,
      select_id = r[[ns_id]]$select_id,
      df = r[[ns_id]]$df,
      map_id = "map"
    )

    # # Update select_id() and df() on bookmark
    # observeEvent(r$sus_bookmark$active, {
    #   if (isTRUE(r$sus_bookmark$active)) {
    #     delay(1000, {
    #       if (!is.null(r$sus_bookmark$select_id))
    #         if (r$sus_bookmark$select_id != "NA") 
    #           select_id(r$sus_bookmark$select_id)
    #       if (!is.null(r$sus_bookmark$df)) df(r$sus_bookmark$df)
    #     })
    #   }
    #   # So that bookmarking gets triggered only ONCE
    #   delay(1500, {
    #     r$sus_bookmark$active <- FALSE
    #     r$sus_bookmark$zoom <- NULL
    #   })
    # }, priority = -2)
    # 
    # # Update select_id() and df() on module link
    # observeEvent(r$sus_link$activity, {
    #   delay(1000, {
    #     if (!is.null(r$sus_link$select_id)) select_id(r$sus_link$select_id)
    #     if (!is.null(r$sus_link$df)) df(r$sus_link$df)
    #     r$sus_link$zoom <- NULL
    #   })
    # }, priority = -2)
    
  })
}
