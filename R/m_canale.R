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
    div(class = "mapdeck_div", rdeckOutput(NS(id, "map"), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), make_dropdown()),
      explore_UI(NS(id, ns_id)), 
      dyk_UI(NS(id, ns_id)))
    
    )
}


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "canale"
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    select_id <- reactiveVal(NA)
    poi <- reactiveVal(NULL)
    
    # Map
    output$map <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = map_zoom)) |> 
        add_mvt_layer(id = ns_id, 
                      data = mvt_url("sus-mcgill.canale-autozoom"),
                      auto_highlight = TRUE, highlight_color = "#FFFFFF80",
                      pickable = TRUE,
                      get_fill_color = scale_fill_sus("canale_ind_2016", "EE"),
                      get_line_color = "#FFFFFF",
                      line_width_units = "pixels", get_line_width = 1)
        })
    
    # Zoom and POI reactives
    observeEvent(input$map_viewstate, {
      zoom(get_zoom(input$map_viewstate$zoom, map_zoom_levels))
      # poi(observe_map(input$map_viewstate))
    })
    
    # Click reactive
    observeEvent(input$map_click, {
      if (!is.na(select_id()) &&
                 input$map_click$data$ID == select_id()) select_id(NA) else {
        select_id(get_click(input$map_click))
      }
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
      var_list = make_dropdown(),
      df = df, 
      time = time)
    
    # Composite variable for map
    map_var <- reactive(
      str_remove(paste(var_left(), var_right(), sep = "_"), "_ $"))
    
    # Sidebar
    sidebar_server(id = ns_id, x = "canale")
    
    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right(),
      new = TRUE))
    
    # Legend
    legend <- legend_server(
      id = ns_id, 
      var_left = var_left, 
      var_right = var_right, 
      df = df,
      zoom = zoom)
    
    # Did-you-know panel
    dyk_server(
      id = ns_id, 
      var_left = var_left,
      var_right = var_right,
      poi = poi)
    
    # Update map in response to variable changes or zooming
    # select_id <- rdeck_change(
    #   id = ns_id,
    #   map_id = NS(id, "map"),
    #   tile = tile,
    #   map_var = map_var,
    #   select_id = select_id
    # )
    

# FUTURE MAP_CHANGE -------------------------------------------------------
    
    observe({
      rdeck_proxy("map") |>
        add_mvt_layer(
          id = ns_id, 
          auto_highlight = TRUE,
          highlight_color = "#FFFFFF80", pickable = TRUE,
          get_fill_color = scale_fill_sus(rlang::sym(map_var()), "EE"),
          get_line_color = "#FFFFFF", line_width_units = "pixels",
          get_line_width = 1)
    })
    
    observeEvent(tile(), {
      observe({
        rdeck_proxy("map") |>
          add_mvt_layer(
            id = ns_id, 
            data = mvt_url(paste0("sus-mcgill.canale-", tile())),
            auto_highlight = TRUE,
            highlight_color = "#FFFFFF80", pickable = TRUE,
            get_fill_color = scale_fill_sus(rlang::sym(map_var()), "EE"),
            get_line_color = "#FFFFFF", line_width_units = "pixels",
            get_line_width = 1)
      })
    })
    
    observeEvent(input$`canale-clear_selection`, select_id(NA))
    observeEvent(df(), select_id(NA), ignoreInit = TRUE)
    # error check
    observeEvent(data(), if (!select_id() %in% data()$ID) select_id(NA),
                 ignoreInit = TRUE)
    
# ----------------------------------------------------------------------------
    
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      zoom = zoom,
      select_id = select_id)
    
    # Data export TKTK should this become a non-reactive function?
    # data_export <- data_export_server(
    #   id = ns_id,
    #   df = data,
    #   var_left = var_left,
    #   var_right = var_right)
    
    # Bookmarking
    # bookmark_server(
    #   id = ns_id,
    #   map_view_change = reactive(input$map_view_change),
    #   var_right = var_right,
    #   select_id = select_id,
    #   df = df,
    #   map_id = NS(id, "map"),
    # )
    
    # Update select_id() on bookmark
    # observeEvent(sus_bookmark$active, {
    #   # Delay of 2000 milliseconds more than the zoom update from bookmark.
    #   # The map/df/data needs to be updated before we select an ID.
    #   if (isTRUE(sus_bookmark$active)) {
    #     delay(2000, {
    #       if (!is.null(sus_bookmark$select_id)) {
    #         if (sus_bookmark$select_id != "NA") select_id(sus_bookmark$select_id)
    #       }
    #     })
    #   }
    #   
    #   # So that bookmarking gets triggered only ONCE
    #   delay(1500, {sus_bookmark$active <- FALSE})      
    # }, priority = -2)
    
    # Update select_id() on module link
    # observeEvent(sus_link$activity, {
    #   # Delay of 2000 milliseconds more than the zoom update from bookmark.
    #   # The map/df/data needs to be updated before we select an ID.
    #   delay(2000, {
    #     if (!is.null(sus_link$select_id)) select_id(sus_link$select_id)
    #   })
    # }, priority = -2)
    
    # OUT
    # reactive({list(
    #   module_short_title = "the CanALE index",
    #   module_id = "canale",
    #   time = "2016",
    #   data = data_export(),
    #   token = map_token,
    #   map_zoom = input$map_view_change$zoom,
    #   map_location = c(input$map_view_change$longitude, 
    #                    input$map_view_change$latitude),
    #   df = df(),
    #   explore_content = explore_content(),
    #   poly_selected = selection())})
    
  })
}
