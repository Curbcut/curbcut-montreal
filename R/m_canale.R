### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"), 
        div(class = "bottom_sidebar", 
            tagList(legend_UI(NS(id, "legend")),
                    zoom_UI(NS(id, "zoom"), map_zoom_levels))))),
    
    fillCol(
      
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id,
        compare_UI(NS(id, "canale"), make_dropdown()),
        div(class = "explore_dyk", 
            explore_UI(NS(id, "explore")), 
            dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial zoom reactive
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))

    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)})
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Zoom level for data
    df <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Time
    time <- reactive("2016")
    
    # Left variable
    var_left <- reactive(paste0("canale_ind_", time()))
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = "canale", 
      var_list = make_dropdown(),
      df = df, 
      time = time)

    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "canale", 
      var_map = reactive(paste0("left_", df(), "_canale_ind_2016")),
      var_right = var_right)
    
    # Data
    data <- reactive(get_data(df(), var_left(), var_right()))
    
    # Legend
    legend <- legend_server(
      id = "legend", 
      var_left = var_left, 
      var_right = var_right, 
      df = df)

    # Did-you-know panel
    dyk_server(
      id = "dyk", 
      var_left = var_left,
      var_right = var_right)

    # Update map in response to variable changes or zooming
    select_id <- map_change(
      id = NS(id, "map"), 
      x = data, 
      df = df, 
      zoom = zoom,
      click = reactive(input$map_polygon_click),
      #legend_selection = reactive(legend()$legend_selection),
      explore_clear = reactive(input$`explore-clear_selection`),
      var_left = var_left,
      var_right = var_right)

    # Explore panel
    explore_content <- explore_server(
      id = "explore", 
      x = data, 
      var_left = var_left,
      var_right = var_right, 
      select_id = select_id,
      df = df, 
      build_str_as_DA = TRUE)
    
    # data naming for data_export
    data_export <- data_export_server(
      id = "canale",
      df = data, 
      var_left = var_left, 
      var_right = var_right)
    
    # OUT
    reactive({list(
      module_short_title = "the CanALE index",
      module_id = "canale",
      time = "2016",
      data = data_export(),
      token = map_token,
      map_zoom = input$map_view_change$zoom,
      map_location = c(input$map_view_change$longitude, 
                       input$map_view_change$latitude),
      df = df(),
      explore_content = explore_content(),
      poly_selected = selection())})
    
  })
}
