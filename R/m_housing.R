### HOUSING MODULE ##############################################################

# UI ----------------------------------------------------------------------

housing_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        select_var_UI(NS(id, "left"), var_list_housing_left), 
        sliderInput(
          NS(id, "slider_uni"), 
          i18n$t("Select a year"),
          min = housing_slider$min,
          max = housing_slider$max,
          step = housing_slider$interval, sep = "",
          value = housing_slider$init,
          width = "95%"),
        sliderInput(
          NS(id, "slider_bi"), 
          i18n$t("Select two years"), 
          min = housing_slider$min,
          max = housing_slider$max, 
          step = housing_slider$interval, sep = "", 
          value = c("2006", "2016"),
          width = "95%"),
        checkboxInput(inputId = NS(id, "slider_switch"),
                      label = i18n$t("Compare dates"), 
                      width = "95%"),
        year_disclaimer_UI(NS(id, "disclaimers")),
        div(class = "bottom_sidebar", 
            tagList(legend_UI(NS(id, "legend")),
                    zoom_UI(NS(id, "zoom"), map_zoom_levels))))),
    
    fillCol(
      
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id,
        compare_UI(NS(id, "housing"), make_dropdown(exclude = "Housing")),
        div(class = "explore_dyk", 
            explore_UI(NS(id, "explore")), 
            dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

housing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))

    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "housing", 
      var_map = reactive(paste0("left_", df(), "_", var_left())))

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
    
    # Enable or disable first and second slider
    observeEvent(input$slider_switch, {
      if (!input$slider_switch) {
        shinyjs::hide("slider_bi") 
        shinyjs::show("slider_uni")
      } else {
        shinyjs::hide("slider_uni")
        shinyjs::show("slider_bi")
      }
    })
    
    # Time variable depending on which slider is active
    time <- reactive({
      if (!input$slider_switch) input$slider_uni else input$slider_bi})
    
    # Greyed out left list options, depending on the year(s) chosen
    var_list_housing_left_disabled <- reactive({
      if (!input$slider_switch) NULL else {
        unlist(var_list_housing_left) %in% {variables |> 
            filter(var_code %in% unlist(var_list_housing_left)) |> 
            filter(!lengths(dates) == max(lengths(dates))) |> 
            pull(var_code)}
      }
    })

    # Left variable server
    var_left <- select_var_server(
      id = "left", 
      var_list = reactive(var_list_housing_left), 
      disabled_choices = var_list_housing_left_disabled,
      time = time, 
      df = df)
    
    # Greyed out right list options, depending of the year chosen
    var_list_housing_right_disabled <- reactive({
      if (!input$slider_switch) NULL else {
        unlist(make_dropdown(exclude = "Housing")) %in% {variables |> 
            filter(var_code %in% unlist(make_dropdown(exclude = "Housing"))) |> 
            filter(!lengths(dates) == max(lengths(dates))) |> 
            pull(var_code)}
      }
    })

    # Right variable server
    var_right <- compare_server(
      id = "housing", 
      var_list = make_dropdown(exclude = "Housing"), 
      disabled_choices = var_list_housing_right_disabled,
      time = time,
      df = df)

    # Data
    data <- data_server(
      id = "housing",
      var_left = var_left,
      var_right = var_right,
      df = df)
    
    # Disclaimers and how to read the map
    year_disclaimer_server(
      id = "disclaimers", 
      data = data,
      var_left = var_left,
      var_right = var_right,
      time = time)
    
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
      x = data,
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
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

  })
}
