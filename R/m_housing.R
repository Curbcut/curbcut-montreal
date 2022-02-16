#### HOUSING MODULE ############################################################

# UI ----------------------------------------------------------------------

housing_UI <- function(id) {
  return(tagList(
      # Sidebar
      sidebar_UI2(
        NS(id, "sidebar"),
        susSidebarWidgets(
          select_var_UI(NS(id, "left"), vars_housing_left), 
          sliderInput(
            NS(id, "slider_uni"), 
            sus_translate("Select a year"),
            min = census_min,
            max = census_max,
            step = 5, sep = "",
            value = census_max,
            width = "95%"),
          sliderInput(
            NS(id, "slider_bi"), 
            sus_translate("Select two years"), 
            min = census_min,
            max = census_max, 
            step = 5, sep = "", 
            value = c("2006", "2016"),
            width = "95%"),
          checkboxInput(
            inputId = NS(id, "slider_switch"),
            label = sus_translate("Compare dates"), 
            width = "95%"),
          year_disclaimer_UI(NS(id, "disclaimer"))
        ),
        bottom=div(class = "bottom_sidebar", 
            tagList(legend_UI(NS(id, "legend")),
                    zoom_UI(NS(id, "zoom"), map_zoom_levels)))),
    
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id,
        compare_UI(NS(id, "housing"), vars_housing_right),
        explore_UI(NS(id, "explore")), 
        dyk_UI(NS(id, "dyk")))
  ))
}


# Server ------------------------------------------------------------------

housing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial zoom reactive
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))

    # Map
    output$map <- renderMapdeck(mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location))
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Zoom level for data
    df <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Enable or disable first and second slider
    observeEvent(input$slider_switch, {
      toggle("slider_uni", condition = !input$slider_switch)
      toggle("slider_bi", condition = input$slider_switch)
    })
    
    # Time variable depending on which slider is active
    time <- reactive({
      if (input$slider_switch) input$slider_bi else input$slider_uni})
    
    # Left variable server
    var_left <- select_var_server(
      id = "left", 
      var_list = reactive(vars_housing_left), 
      disabled = reactive(if (!input$slider_switch) NULL else 
        vars_housing_left_dis),
      time = time, 
      df = df)

    # Right variable/compare panel
    var_right <- compare_server(
      id = "housing", 
      var_list = vars_housing_right, 
      disabled = reactive(if (!input$slider_switch) NULL else 
        vars_housing_right_dis),
      df = df,
      time = time)

    # Sidebar
    sidebar_server(id = "sidebar", x = "housing")
    
    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right()))
    
    # Legend
    legend_server(
      id = "legend",
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      zoom = zoom)
    
    # Did-you-know panel
    dyk_server(
      id = "dyk",
      var_left = var_left,
      var_right = var_right)
    
    # Year disclaimer
    year_disclaimer_server(
      id = "disclaimer", 
      data = data,
      var_left = var_left,
      var_right = var_right)
    
    # Update map in response to variable changes or zooming
    select_id <- map_change(
      NS(id, "map"),
      x = data,
      df = df,
      zoom = zoom,
      click = reactive(input$map_polygon_click),
      #legend_selection = reactive(legend()$legend_selection),
      explore_clear = reactive(input$`explore-clear_selection`)
    )

    # Explore panel
    explore_content <- explore_server(
      id = "explore",
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      zoom = zoom,
      select_id = select_id)

  })
}
