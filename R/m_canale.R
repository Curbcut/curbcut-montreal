### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  ns_id <- "canale"
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, ns_id), 
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, paste0(ns_id, "-map")), 
                                           height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), make_dropdown(compare_default = TRUE)),
      explore_UI(NS(id, ns_id)),
      dyk_UI(NS(id, ns_id)))
    
  )
}


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "canale"
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    
    # Map
    output[[paste0(ns_id, "-map")]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = map_zoom)) |> 
        add_mvt_layer(id = ns_id) |> 
        add_mvt_layer(id = paste0(ns_id, "_street_1")) |> 
        add_mvt_layer(id = paste0(ns_id, "_street_2")) |> 
        add_mvt_layer(id = paste0(ns_id, "_street_2")) |> 
        add_mvt_layer(id = paste0(ns_id, "_building")) |> 
        add_mvt_layer(id = paste0(ns_id, "_borough_labels"))
    })
    
    # Zoom and POI reactives
    observeEvent(get_view_state(paste0(ns_id, "-map")), {
      zoom(get_zoom(get_view_state(paste0(ns_id, "-map"))$zoom))
      poi(observe_map(get_view_state(paste0(ns_id, "-map"))))
    })
    
    # Click reactive
    observeEvent(get_clicked_object(paste0(ns_id, "-map")), {
      selection <- get_clicked_object(paste0(ns_id, "-map"))$ID
      if (!is.na(select_id()) && selection == select_id()) return(select_id(NA))
      
      select_id(selection)
    })
    
    # Choose tileset
    tile <- zoom_server(
      id = ns_id, 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Get df for explore/legend/etc
    df <- reactive(get_df(tile(), zoom(), map_zoom_levels))
    
    # Time
    time <- reactive("2016")
    
    # Left variable
    var_left <- reactive(paste0("canale_ind_", time()))
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = ns_id, 
      var_list = make_dropdown(compare_default = TRUE),
      df = df, 
      time = time)
    
    # Additional tileset identifier
    tile2 <- reactive("")
    
    # Composite variable for map
    map_var <- reactive(
      str_remove(paste(var_left(), var_right(), sep = "_"), "_ $"))
    
    # Sidebar
    sidebar_server(id = ns_id, x = "canale")
    
    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right()))
    
    # Legend
    legend <- legend_server(
      id = ns_id, 
      var_left = var_left, 
      var_right = var_right, 
      df = df)
    
    # Did-you-know panel
    dyk_server(
      id = ns_id, 
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    # Update map in response to variable changes or zooming
    rdeck_server(
      id = ns_id, 
      map_id = "map", 
      tile = tile,
      tile2 =  tile2,
      map_var = map_var, 
      zoom = zoom,
      select_id = select_id
    )
    
    # De-select
    observeEvent(input[[paste0(ns_id, "-clear_selection")]], select_id(NA))
    observeEvent(df(), select_id(NA), ignoreInit = TRUE)
    # Error check
    observeEvent(data(), if (!select_id() %in% data()$ID) select_id(NA),
                 ignoreInit = TRUE)
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      select_id = select_id)
    
    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_viewstate = reactive(get_view_state(paste0(ns_id, "-map"))),
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = "map",
    )
    
    # Update select_id() on bookmark
    observeEvent(sus_bookmark$active, {
      if (isTRUE(sus_bookmark$active)) {
        if (!is.null(sus_bookmark$df)) df <- reactiveVal(sus_bookmark$df)
        delay(1000, {
          if (!is.null(sus_bookmark$select_id))
            if (sus_bookmark$select_id != "NA") 
              select_id(sus_bookmark$select_id)
        })
      }
      # So that bookmarking gets triggered only ONCE
      delay(1500, {sus_bookmark$active <- FALSE})
    }, priority = -2)
    
    # Update select_id() on module link
    observeEvent(sus_link$activity, {
      if (!is.null(sus_bookmark$df)) df <- reactiveVal(sus_bookmark$df)
      delay(1000, {
        if (!is.null(sus_link$select_id)) select_id(sus_link$select_id)
      })
    }, priority = -2)
    
  })
}
