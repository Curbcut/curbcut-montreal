### CRASH MODULE ###############################################################

# UI ----------------------------------------------------------------------

crash_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        hr(),
        actionLink(NS(id, "analysis"), 
                   i18n$t("Road safety analysis")),
        hr(id = NS(id, "hr")),
        select_var_UI(NS(id, "left_1"), var_list_left_crash_1,
                      label = i18n$t("Grouping")),
        select_var_UI(NS(id, "left_2"), var_list_left_crash_2,
                      label = i18n$t("Type of crash")),
        div(id = NS(id, "slider"),
            sliderInput(NS(id, "left"), 
                        i18n$t("Select a year"),
                        min = crash_slider$min,
                        max = crash_slider$max,
                        step = crash_slider$interval, sep = "",
                        value = crash_slider$init,
                        width = "95%"),
            sliderInput(NS(id, "left_bi_time"), 
                        i18n$t("Select two dates"), 
                        min = crash_slider$min,
                        max = crash_slider$max, 
                        step = crash_slider$interval, sep = "", 
                        value = c("2012", "2019"),
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
      
      # Crash analysis
      shinyjs::hidden(htmlOutput(
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
            dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

crash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_crash_levels))
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
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_crash_levels))})
    
    # Zoom level for data
    df_choropleth <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = map_zoom_levels)
    
    df <- reactive({if (input$grid) "grid" else df_choropleth()})
    
    # Enable or disable first and second slider
    observeEvent(input$bi_time, {
      shinyjs::toggle("left_bi_time", condition = input$bi_time)
      shinyjs::toggle("left", condition = !input$bi_time)})
    
    # If we aren't in choropleth, toggle off the zoom and grid checkbox
    observeEvent(choropleth(), {
      shinyjs::toggle("zoom-auto", condition = choropleth())
      shinyjs::toggle("zoom-slider", condition = choropleth())
      shinyjs::toggle("grid", condition = choropleth())
    })
    
    # Time variable depending on which slider
    time <- reactive({if (!input$bi_time) input$left else input$left_bi_time})
    
    # Left variable servers
    var_left_1 <- select_var_server("left_1", reactive(var_list_left_crash_1))
    var_left_2 <- select_var_server("left_2", reactive(var_list_left_crash_2))
    
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
    data_choropleth <- data_server(
      id = "crash", 
      var_left = var_left,
      var_right = var_right, 
      df = df, 
      island = TRUE)
    
    data <- reactive({
      if (choropleth()) {
        data_choropleth()
      } else {
        crash %>%
          { if (var_left_2() %in% unique(crash$type))
            filter(., type == var_left_2()) else .} %>%
          { if (length(time()) == 2) {
            filter(., year %in% time()[1]:time()[2])
          } else {
            filter(., year == time())
          }}     
      }
    })
    
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
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore",
      x = data_for_explore,
      var_left = var_left,
      var_right = var_right_for_exp,
      select = selection,
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
    
    # Update map in response to variable changes or zooming
    map_change(NS(id, "map"), 
               x = data, 
               df = df, 
               selection = selection,
               legend = crash_legend_en)
    
    # Update poly on click
    observeEvent(input$map_polygon_click, {
      lst <- (jsonlite::fromJSON(input$map_polygon_click))$object$properties$id
      if (is.null(lst)) selection(NA) else selection(lst)
    })
    
    # Update point on click
    observeEvent(input$map_scatterplot_click, {
      lst <- jsonlite::fromJSON(input$map_scatterplot_click)$index
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
      
      shinyjs::toggle("hr", condition = !input$analysis %% 2)
      shinyjs::toggle("left_1-var", condition = !input$analysis %% 2)
      shinyjs::toggle("left_2-var", condition = !input$analysis %% 2)
      shinyjs::toggle("slider", condition = !input$analysis %% 2)
      shinyjs::toggle("slider_switch", condition = !input$analysis %% 2)
      shinyjs::toggle("right_panel", condition = !input$analysis %% 2)
      shinyjs::toggle("how_to_read_map", condition = !input$analysis %% 2)
      shinyjs::toggle("year_displayed_right", condition = !input$analysis %% 2)
      shinyjs::toggle("legend-legend_render", condition = !input$analysis %% 2)
      shinyjs::toggle("zoom-auto", condition = !input$analysis %% 2)
      shinyjs::toggle("zoom-slider", condition = !input$analysis %% 2)
      shinyjs::toggle("grid", condition = !input$analysis %% 2)
      shinyjs::toggle("crash_analysis", condition = input$analysis %% 2)
      
    })
  })
}
