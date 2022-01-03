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
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    selection <- reactiveVal(NA)
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "canale", 
      var_map = reactive(paste0("left_", df(), "_", "canale_ind_2016")))
    
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
      zoom_levels = map_zoom_levels)
    
    # Time
    time <- reactive("2016")
    
    # Left variable
    var_left <- reactive("canale_ind_2016")
    
    # Compare panel
    var_right <- compare_server(
      id = "canale", 
      var_list = make_dropdown(),
      df = df, 
      time = time)

    # Data
    data <- data_server(
      id = "canale", 
      var_left = var_left,
      var_right = var_right, 
      df = df, 
      zoom = zoom)
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore", 
      x = data, 
      var_left = var_left,
      var_right = var_right, 
      selection = selection,
      df = df, 
      build_str_as_DA = TRUE)

    # Legend
    legend_server(
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
    map_change(NS(id, "map"), 
               x = data, 
               df = df, 
               selection = selection)

    # Update poly on click
    observeEvent(input$map_polygon_click, {
      lst <- (jsonlite::fromJSON(input$map_polygon_click))$object$properties$id
      if (is.null(lst)) selection(NA) else selection(lst)
    })
    
    # Clear selection on df change
    observeEvent(df(), selection(NA), ignoreInit = TRUE)

    # Clear click status if prompted
    observeEvent(input$`explore-clear_selection`, selection(NA))
    
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
