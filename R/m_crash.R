### CRASH MODULE ###############################################################

# UI ----------------------------------------------------------------------

crash_UI <- function(id) {
  return(tagList(
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        hr(),
        actionLink(NS(id, "analysis"), 
                   sus_translate("Road safety analysis")),
        hr(id = NS(id, "hr")),
        select_var_UI(NS(id, "left_1"), var_list = var_list_left_crash_1,
                      label = sus_translate("Grouping")),
        select_var_UI(NS(id, "left_2"), var_list = var_list_left_crash_2,
                      label = sus_translate("Type of crash")),
        div(id = NS(id, "slider"),
            sliderInput(NS(id, "left"), 
                        sus_translate("Select a year"),
                        min = crash_slider$min,
                        max = crash_slider$max,
                        step = crash_slider$interval, sep = "",
                        value = crash_slider$init,
                        width = "95%"),
            sliderInput(NS(id, "left_bi_time"), 
                        sus_translate("Select two dates"), 
                        min = crash_slider$min,
                        max = crash_slider$max, 
                        step = crash_slider$interval, sep = "", 
                        value = c("2012", "2019"),
                        width = "95%")),
        div(id = NS(id, "slider_switch"),
            checkboxInput(inputId = NS(id, "bi_time"),
                          label = sus_translate("Compare dates"))),
        hidden(checkboxInput(
          inputId = NS(id, "grid"), 
          label = sus_translate("250-metre grid"))),
        year_disclaimer_UI(NS(id, "disclaimers")),
        div(class = "bottom_sidebar", 
            tagList(legend_UI(NS(id, "legend")),
                    zoom_UI(NS(id, "zoom"), map_zoom_levels)))),

      # Crash analysis
      hidden(htmlOutput(
        NS(id, "crash_analysis"),
        style = paste0("position:absolute; margin: 40px; ",
                       "max-width: 1000px; z-index:499"))),
      
      # Map
      div(class = "mapdeck_div", 
          mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id, 
        compare_UI(NS(id, "crash"), make_dropdown()),
        div(class = "explore_dyk", 
            explore_UI(NS(id, "explore")), 
            dyk_UI(NS(id, "dyk"))))
  ))
}


# Server ------------------------------------------------------------------

crash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, c("heatmap" = 0, "point" = 12)))
    selection <- reactiveVal(NA)
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "crash", 
      var_map = reactive(paste0("left_", df(), "_", var_left())))
    
    # If COUNT isn't selected, choropleth is TRUE 
    choropleth <- reactive(var_left_1() != "count")
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)})
    
    # Zoom reactive
    map_zoom_levels_crash <- reactive({
      if (choropleth()) map_zoom_levels else c("heatmap" = 0, "point" = 12)
    })
    
    observeEvent({input$map_view_change$zoom
      map_zoom_levels_crash()}, {
        actual_zoom <- if (is.null(input$map_view_change$zoom)) map_zoom else {
          input$map_view_change$zoom
        }
        zoom(get_zoom(actual_zoom, map_zoom_levels_crash()))}, 
      ignoreInit = TRUE)
    
    # Zoom level for data
    df_choropleth <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = map_zoom_levels_crash)
    
    df <- reactive({if (input$grid) "grid" else df_choropleth()})
    
    # Enable or disable first and second slider
    observeEvent(input$bi_time, {
      toggle("left_bi_time", condition = input$bi_time)
      toggle("left", condition = !input$bi_time)})
    
    # If we aren't in choropleth, toggle off the zoom and grid checkbox
    observeEvent(choropleth(), {
      toggle("grid", condition = choropleth())
    })
    
    # Time variable depending on which slider
    time <- reactive({if (!input$bi_time) input$left else input$left_bi_time})
    
    # Left variable servers
    var_left_1 <- select_var_server("left_1", var_list = reactive(var_list_left_crash_1))
    var_left_2 <- select_var_server("left_2", var_list = reactive(var_list_left_crash_2))
    
    # Construct left variable string
    var_left <- reactive({
      str_remove(paste("crash", 
                       var_left_2(), 
                       var_left_1(), 
                       time(), sep = "_"), "_ ")
    })
    
    # Compare panel
    var_right <- compare_server(
      id = "crash", 
      var_list = make_dropdown(), 
      df = df, 
      time = time,
      show_panel = choropleth)
    
    # Data 
    data <- reactive(get_data(df(), var_left(), var_right(), island = TRUE,
                              point_df = "crash"))
    
    # Disclaimers and how to read the map
    year_disclaimer_server(
      id = "disclaimers", 
      data = data,
      var_left = var_left,
      var_right = var_right,
      time = time,
      pct_variation = choropleth)

    # Prepare different type of values for the explore panel
    data_for_explore <- reactive({
      if (choropleth()) data() else {
        data() |>
          st_drop_geometry() |>
          count(date) |>
          rename(var_left = n, var_right = date) |>
          mutate(ID = seq_along(var_left), .before = var_left) |>
          mutate(var_left_q3 = var_left, var_left_q5 = var_left)
      }
    })
    df_for_explore <- reactive({if (choropleth()) df() else "date"})
    var_right_for_exp <- reactive({if (choropleth()) var_right() else "date"})
    
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
      x = data_for_explore,
      var_left = var_left,
      var_right = var_right_for_exp,
      select_id = select_id,
      df = df_for_explore)
    
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

    # Update point on click
    observeEvent(input$map_scatterplot_click, {
      lst <- fromJSON(input$map_scatterplot_click)$index
      if (is.null(lst)) selection(NA) else {
        # This is a hack because of a mapdeck bug
        selection(crash[lst + 1,]$ID)
      }
    })
    
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
    
    # Crash anaylsis pop-up
    output$crash_analysis <- renderUI(
      tags$iframe(src = "crash/crash.html", width = "1000px", height = "800px",
                  style = "max-height: 83vh; overflow: auto; background-color: #fff;
                    border: 1px solid transparent; border-radius: 4px;
                    box-shadow: 0 50px 50px rgba(0,0,0,.6);")
    )
    
    observeEvent(input$analysis, {
      
      if (input$analysis %% 2 == 1) {
        txt <- sus_translate("Road safety map") 
      } else txt <- sus_translate("Road safety analysis")
      
      updateActionLink(session, "analysis", label = txt)
      
      toggle("hr", condition = !input$analysis %% 2)
      toggle("left_1-var", condition = !input$analysis %% 2)
      toggle("left_2-var", condition = !input$analysis %% 2)
      toggle("slider", condition = !input$analysis %% 2)
      toggle("slider_switch", condition = !input$analysis %% 2)
      toggle("right_panel", condition = !input$analysis %% 2)
      toggle("how_to_read_map", condition = !input$analysis %% 2)
      toggle("year_displayed_right", condition = !input$analysis %% 2)
      toggle("legend-legend_render", condition = !input$analysis %% 2)
      toggle("zoom-auto", condition = !input$analysis %% 2)
      toggle("zoom-slider", condition = !input$analysis %% 2)
      toggle("grid", condition = !input$analysis %% 2)
      toggle("crash_analysis", condition = input$analysis %% 2)
      
    })
  })
}
