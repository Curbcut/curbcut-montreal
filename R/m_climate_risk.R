### CLIMATE RISK MODULE ########################################################

# UI ----------------------------------------------------------------------

climate_risk_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        select_var_UI(NS(id, "left"), var_list_climate_risk), 
        checkboxInput(
          inputId = NS(id, "grid"), value = TRUE,
          label = i18n$t("250-metre grid")),
        div(class = "bottom_sidebar",
            tagList(legend_UI(NS(id, "legend")),
                    zoom_UI(NS(id, "zoom"), map_zoom_levels))))),
    
    fillCol(
      
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id, 
        compare_UI(NS(id, "climate_risk"), make_dropdown()),
        div(class = "explore_dyk",
            explore_UI(NS(id, "explore")),
            dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

climate_risk_server <- function(id) {
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
    df_choropleth <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # String to fetch maps and data
    df <- reactive(if (input$grid) "grid" else df_choropleth())
    
    # Time
    time <- reactive("2016")
    
    # Left variable server
    var_left <- select_var_server("left", reactive(var_list_climate_risk))
    
    # Right variable/compare panel
    var_right <- compare_server(
      id = "climate_risk", 
      var_list = make_dropdown(),
      df = df, 
      time = time)
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "climate_risk", 
      var_map = reactive(paste0("left_", df(), "_", var_left())),
      var_right = var_right)
    
    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right(), 
      island = TRUE))
    
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
      explore_clear = reactive(input$`explore-clear_selection`))
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore",
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      select_id = select_id)
    
    # If grid isn't clicked, toggle on the zoom menu
    observeEvent(input$grid, {
      shinyjs::toggle("zoom-auto", condition = !input$grid)
      shinyjs::toggle("zoom-slider", condition = !input$grid)
    })
  })
}
