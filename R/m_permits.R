### PERMITS MODULE #############################################################

# UI ----------------------------------------------------------------------

permits_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        select_var_UI(NS(id, "left_1"), var_list_left_permits_1,
                      label = i18n$t("Grouping")),
        select_var_UI(NS(id, "left_2"), var_list_left_permits_2,
                      label = i18n$t("Type of permits")),
        div(id = NS(id, "slider"),
            sliderInput(NS(id, "left"), 
                        i18n$t("Select a year"),
                        min = permits_slider$min,
                        max = permits_slider$max,
                        step = permits_slider$interval, sep = "",
                        value = permits_slider$init,
                        width = "95%"),
            sliderInput(NS(id, "left_bi_time"), 
                        i18n$t("Select two dates"), 
                        min = permits_slider$min,
                        max = permits_slider$max, 
                        step = permits_slider$interval, sep = "", 
                        value = c("2000", "2021"),
                        width = "95%")),
        div(id = NS(id, "slider_switch"),
            checkboxInput(inputId = NS(id, "bi_time"),
                          label = i18n$t("Compare dates"))),
        shinyjs::hidden(checkboxInput(
          inputId = NS(id, "grid"), 
          label = i18n$t("250-metre grid"))),
        year_disclaimer_UI(NS(id, "disclaimers")),
        div(class = "bottom_sidebar", 
            tagList(legend_UI(NS(id, "legend")),
                    zoom_UI(NS(id, "zoom"), map_zoom_levels))))),
    
    fillCol(
      
      # Map
      div(class = "mapdeck_div", 
          mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id, 
        compare_UI(NS(id, "permits"), make_dropdown()),
        div(class = "explore_dyk", 
            explore_UI(NS(id, "explore")), 
            dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

permits_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, c("heatmap" = 0, "point" = 12)))
    selection <- reactiveVal(NA)
    
    # Load heavy data used by this module
    if (any(!str_detect("permits_combination_count_1990", names(borough)))) {
      qload("data/permits.qsm")
      borough <<- bind_cols(borough, permits_choropleth$borough)
      CT <<- bind_cols(CT, permits_choropleth$CT)
      DA <<- bind_cols(DA, permits_choropleth$DA)
      grid <<- bind_cols(grid, permits_choropleth$grid)
      permits <<- permits
    }
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "permits", 
      var_map = reactive(paste0("left_", df(), "_", var_left())))
    
    # If COUNT isn't selected, choropleth is TRUE 
    choropleth <- reactive(var_left_1() != "count")

    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)})
    
    # Enable or disable first and second slider
    observeEvent(input$bi_time, {
      shinyjs::toggle("left_bi_time", condition = input$bi_time)
      shinyjs::toggle("left", condition = !input$bi_time)})
    
    # If we aren't in choropleth, toggle off the zoom and grid checkbox
    observeEvent(choropleth(), {
      shinyjs::toggle("grid", condition = choropleth())
    })
    
    # If we aren't in choropleth, toggle off the zoom and grid checkbox
    observeEvent(input$grid, {
      shinyjs::toggle("zoom-auto", condition = !input$grid)
      shinyjs::toggle("zoom-slider", condition = !input$grid)
    })
    
    # Time variable depending on which slider
    time <- reactive({if (!input$bi_time) input$left else input$left_bi_time})
    
    # Left variable servers
    var_left_1 <- select_var_server("left_1", reactive(var_list_left_permits_1))
    var_left_2 <- select_var_server("left_2", reactive(var_list_left_permits_2))
    
    # Construct left variable string
    var_left <- reactive({
      str_remove(paste("permits", 
                       var_left_2(), 
                       var_left_1(), 
                       time(), sep = "_"), "_ ")
    })
    
    # Zoom reactive
    map_zoom_levels_permits <- reactive({
      if (choropleth()) map_zoom_levels else c("heatmap" = 0, "point" = 12)
    })
    
    observeEvent({input$map_view_change$zoom
      map_zoom_levels_permits()}, {
        actual_zoom <- if (is.null(input$map_view_change$zoom)) map_zoom else {
          input$map_view_change$zoom
        }
        zoom(get_zoom(actual_zoom, map_zoom_levels_permits()))}, 
      ignoreInit = TRUE)

    # Zoom level for data
    df_choropleth <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = map_zoom_levels_permits)        

    df <- reactive({if (input$grid) "grid" else df_choropleth()})
    
    # Compare panel
    var_right <- compare_server(
      id = "permits", 
      var_list = make_dropdown(), 
      df = df,
      time = time,
      show_panel = choropleth)
    
    data <- reactive(get_data(df(), var_left(), var_right(), island = TRUE,
                              point_df = "permits"))
    
    # Disclaimers and how to read the map
    year_disclaimer_server(
      id = "disclaimers",
      data = data,
      var_left = var_left,
      var_right = var_right,
      time = time,
      pct_variation = choropleth,
      # If the same time is selected twice, other disclaimer
      more_condition = reactive(!choropleth() && all(is.na(pull(data(), ID)))),
      more_text = paste0(
        "<p style='font-size:11px;'>",
        "There is no '{var_left_title}' to report for ",
        "{left_year}.</p>"))
    
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
    
    # Explore select
    # Update point on click
    observeEvent(input$map_scatterplot_click, {
      lst <- jsonlite::fromJSON(input$map_scatterplot_click)$index
      if (is.null(lst)) selection(NA) else {
        # This is a hack because of a mapdeck bug
        selection(data()[lst + 1,]$ID)
      }
    })
    
    current_select <- reactive(if (choropleth()) select_id() else selection())
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore",
      x = data,
      var_left = var_left,
      var_right = var_right,
      select = current_select,
      df = df,
      standard = choropleth,
      custom_info = permits_info_table,
      custom_graph = permits_explore_graph)
    
    # Legend
    legend_server(
      id = "legend", 
      var_left = var_left, 
      var_right = var_right, 
      df = df,
      show_panel = choropleth)
    
    # Did-you-know panel
    dyk_server(
      id = "dyk", 
      var_left = var_left,
      var_right = var_right)
    
    # Clear selection on df change
    observeEvent(df(), selection(NA), ignoreInit = TRUE)
    
    # Clear click status if prompted
    observeEvent(input$`explore-clear_selection`, selection(NA))

    # Bi slider label explained
    observe({
      if (!choropleth()) {
        updateSliderInput(session, inputId = "left_bi_time", 
                          label = sus_translate("Total between two dates"))
      } else if (choropleth()) {
        updateSliderInput(session, inputId = "left_bi_time", 
                          label = sus_translate("Compare two dates"))
      }
    })
  })
}
