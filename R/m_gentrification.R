### GENTRIFICATION MODULE #####################################################

# UI ----------------------------------------------------------------------

gentrification_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        sliderInput(
          NS(id, "slider_time"), 
          sus_translate("Select two years"),
          min = gentrification_slider$min,
          max = gentrification_slider$max,
          step = gentrification_slider$interval, sep = "",
          value = gentrification_slider$init,
          width = "95%"),
        checkboxInput(NS(id, "check_single_var"),
                      label = sus_translate("Review a single variable ",
                                            "part of the index")),
        select_var_UI(NS(id, "left"), 
                      var_list_left_gentrification),
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
        compare_UI(NS(id, "gentrification"), make_dropdown(multi_year = T)),
        div(class = "explore_dyk", 
            explore_UI(NS(id, "explore")), 
            dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

gentrification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
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
    
    # Get time from slider
    time <- reactive({input$slider_time})
    
    observe({
      if (length(unique(time())) == 1 && !input$check_single_var) {
        shinyalert::shinyalert(text = paste0(
          "Gentrification is a process that ",
          "can only be quantified over time. ",
          " Please, select two different years."), 
          type = "error")
      }
    })
    
    # Greyed out right list options, depending of the year chosen
    var_list_housing_right_disabled <- reactive({
      if (!input$check_single_var) NULL else {
        unlist(make_dropdown(multi_year = T)) %in% var_list_left_gentrification
      }
    })
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = "gentrification", 
      var_list = make_dropdown(multi_year = TRUE),
      disabled = var_list_housing_right_disabled,
      df = df,
      time = time)
    
    # Get single_var value to use if check_single_var is clicked
    single_var <- select_var_server(
      id = "left", 
      var_list = reactive(var_list_left_gentrification), 
      time = time, 
      df = df)
    
    # If check_single_var is clicked, toggle on the dropdown menu
    observeEvent(input$check_single_var, {
      toggle("left-var", condition = input$check_single_var)
    })
    
    # Construct left variable string
    var_left <- reactive({
      if (!input$check_single_var) {
        str_remove(paste("gentrification_ind", time(), sep = "_"), "_ $")
      } else {
        single_var()
      }
    })
    
    # Sidebar
    sidebar_server(id = "sidebar", x = "gentrification")
    
    # Data
    data <- reactive(get_data(df(), var_left(), var_right()))
    
    # # Disclaimers and how to read the map
    # year_disclaimer_server(
    #   id = "disclaimers", 
    #   data = data,
    #   var_left = var_left,
    #   var_right = var_right,
    #   # If the same time is selected twice, other disclaimer
    #   more = reactive({length(unique(time())) == 1 && 
    #       !input$check_single_var}),
    #   more_text = paste0(
    #     "<p style='font-size:11px;'>",
    #     "Gentrification is a process that can only be quantified over time. ",
    #     "Please, select two different years.</p>"))
    
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
    
    # Legend
    legend <- legend_server(
      id = "legend",
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df)
    
    # Did-you-know panel
    dyk_server(
      id = "dyk",
      var_left = var_left,
      var_right = var_right)
    
    # Bookmarking 
    onBookmark(function(state) {
      state$values$zoom_val <- zoom_val()
      state$values$numeric_zoom <- input$map_view_change$zoom
      state$values$location <- c(input$map_view_change$longitude, 
                                 input$map_view_change$latitude)
      state$values$poly_selected <- rv_gentrification$poly_selected
      state$values$var_right <- var_right()
    })
    
    onRestored(function(state) {
      restored_numeric_zoom <- state$values$numeric_zoom
      restored_map_location <- state$values$location
      zoom_val(state$values$zoom_val)
      
      output$map <- renderMapdeck({
        mapdeck(
          style = map_style, token = token_gentrification,
          zoom = restored_numeric_zoom, location = restored_map_location)
      })
      
      updatePickerInput(
        session = session,
        inputId = NS(id, "compare-var"),
        choices = sus_translate(make_dropdown(multi_year = T)),
        selected = unique(str_remove(state$values$var_right, "_\\d{4}"))
      )
      
      rv_gentrification$poly_selected <- state$values$poly_selected
    })
    
    # data naming for data_export
    data_export <- data_export_server(id = "gentrification",
                                      df = data, var_left = var_left, 
                                      var_right = var_right)
    
    # OUT
    reactive({list(module_short_title = "gentrification",
                   module_id = "gentrification",
                   time = time(),
                   data = data_export(),
                   token = token_gentrification,
                   map_zoom = input$map_view_change$zoom,
                   map_location = c(input$map_view_change$longitude, 
                                    input$map_view_change$latitude),
                   zoom = zoom(),
                   explore_content = explore_content(),
                   poly_selected = rv_gentrification$poly_selected,
                   legend_graph = legend_graph())})
    
  })
}
