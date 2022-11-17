### CRASH MODULE ###############################################################

# UI ----------------------------------------------------------------------

crash_UI <- function(id) {
  ns_id <- "crash"
  
  return(tagList(
    # Sidebar
    sidebar_UI(
      NS(id, ns_id),
      susSidebarWidgets(
        actionLink(NS(id, "analysis"), 
                   cc_t(r = r, "Road safety analysis")),
        select_var_UI(NS(id, ns_id), select_var_id = "d_2",
                      var_list = var_left_list_2_crash,
                      label = cc_t(r = r, "Grouping")),
        select_var_UI(NS(id, ns_id), select_var_id = "d_1",
                      var_list = var_left_list_1_crash,
                      label = cc_t(r = r, "Type of crash")),
        
        slider_UI(NS(id, ns_id), 
                  slider_id = "slu",
                  min = min(crash$year),
                  max = max(crash$year),
                  step = 1, sep = "",
                  value = max(crash$year)),
        slider_UI(NS(id, ns_id), 
                  slider_id = "slb",
                  label = cc_t(r = r, "Select two years (data aggregate)"), 
                  min = min(crash$year),
                  max = max(crash$year), 
                  step = 1, sep = "", 
                  value = c("2012", "2019")),
        
        checkbox_UI(NS(id, ns_id),
                    checkbox_id = "comp_d",
                    label = cc_t(r = r, "Compare dates")),
        hidden(checkbox_UI(NS(id, ns_id),
                           checkbox_id = "grid",
                           label = cc_t(r = r, "250-metre grid"))),
        year_disclaimer_UI(NS(id, ns_id))
      ),
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels_CMA)))),
    
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
      compare_UI(NS(id, ns_id), make_dropdown(compare = TRUE)),
      explore_UI(NS(id, ns_id)), 
      dyk_UI(NS(id, ns_id)))
  ))
}


# Server ------------------------------------------------------------------

crash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "crash"
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, c("heatmap" = 0, "point" = 12)))
    selection <- reactiveVal(NA)
    click_id <- reactiveVal(NULL)
    
    # Click reactive
    observeEvent(input$map_polygon_click, {
      click_id(get_click(input$map_polygon_click))})
    
    # Sidebar
    sidebar_server(id = ns_id, x = "crash")
    
    # If COUNT isn't selected, choropleth is TRUE 
    choropleth <- reactive(var_left_1() != "count")
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_loc)})
    
    # Zoom reactive
    map_zoom_levels_crash <- reactive({
      if (choropleth()) map_zoom_levels_CMA else c("heatmap" = 0, "point" = 12)
    })
    
    observeEvent({input$map_view_change$zoom
      map_zoom_levels_crash()}, {
        actual_zoom <- if (is.null(input$map_view_change$zoom)) map_zoom else {
          input$map_view_change$zoom
        }
        zoom(get_zoom(actual_zoom, map_zoom_levels_crash()))},
      ignoreInit = TRUE)

    # Checkbox values
    bi_time <- checkbox_server(id = ns_id, checkbox_id = "comp_d")
    cbox_grid <- checkbox_server(id = ns_id, checkbox_id = "grid")

    # Enable or disable first and second slider
    slider_uni <- slider_server(id = ns_id, slider_id = "slu")
    slider_bi <- slider_server(id = ns_id, slider_id = "slb")

    # Zoom level for data
    df_choropleth <- zoom_server(
      id = ns_id,
      zoom = zoom,
      zoom_levels = map_zoom_levels_crash)

    df <- reactive({if (cbox_grid() && choropleth()) "grid" else df_choropleth()})

    # Time variable depending on which slider
    time <- reactive({if (!bi_time()) slider_uni() else slider_bi()})

    # Left variable servers
    var_left_1 <- select_var_server(ns_id, select_var_id = "d_2",
                                    var_list = reactive(var_left_list_2_crash))
    var_left_2 <- select_var_server(ns_id, select_var_id = "d_1",
                                    var_list = reactive(var_left_list_1_crash))

    # Construct left variable string
    var_left <- reactive({
      str_remove(paste("crash",
                       var_left_2(),
                       var_left_1(),
                       time(), sep = "_"), "_ ")
    })

    # Compare panel
    var_right <- compare_server(
      id = ns_id,
      var_list = make_dropdown(compare = TRUE),
      df = df,
      time = time,
      show_panel = choropleth)

    # Data
    data <- reactive(get_data(df(), var_left(), var_right(), island = TRUE,
                              point_df = "crash"))

    # Disclaimers and how to read the map
    year_disclaimer_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right)
      # time = time,
      # pct_variation = choropleth)

    # Prepare different type of values for the explore panel
    # data_for_explore <- reactive({
    #   if (choropleth()) data() else {
    #     data() |>
    #       count(date) |>
    #       rename(var_left = n, var_right = date) |>
    #       mutate(ID = seq_along(var_left), .before = var_left) |>
    #       mutate(var_left_q3 = var_left, var_left_q5 = var_left)
    #   }
    # })
    # df_for_explore <- reactive({if (choropleth()) df() else "date"})
    # var_right_for_exp <- reactive({if (choropleth()) var_right() else "date"})

    # Update map in response to variable changes or zooming
    select_id <- map_change(
      id = ns_id,
      map_id = NS(id, "map"),
      data = data,
      df = df,
      zoom = zoom,
      click = click_id,
    )

    # # Explore panel
    # explore_content <- explore_server(
    #   id = ns_id,
    #   x = data_for_explore,
    #   var_left = var_left,
    #   var_right = var_right_for_exp,
    #   select_id = select_id,
    #   df = df_for_explore)

    # Legend
    # legend_server(
    #   id = ns_id,
    #   var_left = var_left,
    #   var_right = var_right,
    #   df = df,
    #   show_panel = choropleth)

    # Did-you-know panel
    dyk_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right)

    # Update point on click
    observeEvent(input$map_scatterplot_click, {
      lst <- fromJSON(input$map_scatterplot_click)$index
      if (lst == 0) selection(NA) else {
        # This is a hack because of a mapdeck bug
        selection(crash[lst + 1,]$ID)
      }
    })

    # Clear selection on df change
    observeEvent(df(), selection(NA), ignoreInit = TRUE)

    # Clear click status if prompted
    observeEvent(input$`crash-clear_selection`, selection(NA))

    # Bi slider label explained
    observe({
      if (!choropleth()) {
        updateSliderInput(session, inputId = "crash-slb",
                          label = cc_t(r = r, "Total between two dates"))
      } else if (choropleth()) {
        updateSliderInput(session, inputId = "crash-slb",
                          label = cc_t(r = r, "Compare two dates"))
      }
    })

    # Crash analysis pop-up
    output$crash_analysis <- renderUI(
      tags$iframe(src = "crash/crash.html", width = "1000px", height = "800px",
                  style = "max-height: 83vh; overflow: auto; background-color: #fff;
                    border: 1px solid transparent; border-radius: 4px;
                    box-shadow: 0 50px 50px rgba(0,0,0,.6);")
    )

    # JS toggle on and off
    observeEvent(input$analysis, {

      if (input$analysis %% 2 == 1) {
        txt <- cc_t(r = r, "Road safety map")
      } else txt <- cc_t(r = r, "Road safety analysis")

      updateActionLink(session, "analysis", label = txt)

      toggle("crash-d_1", condition = !input$analysis %% 2)
      toggle("crash-d_2", condition = !input$analysis %% 2)
      toggle("right_panel", condition = !input$analysis %% 2)
      toggle("how_to_read_map", condition = !input$analysis %% 2)
      toggle("year_displayed_right", condition = !input$analysis %% 2)
      toggle("crash-legend_render", condition = !input$analysis %% 2)
      toggle("crash-comp_d", condition = !input$analysis %% 2)
      toggle("crash_analysis", condition = input$analysis %% 2)

    })
    
    observe({
      if (!input$analysis %% 2 || input$analysis == 0) {
        toggle("crash-slb", condition = bi_time())
        toggle("crash-slu", condition = !bi_time())
        toggle("crash-grid", condition = choropleth())
        toggle("crash-zoom_auto", condition = !cbox_grid())
        toggle("crash-zoom_slider", condition = !cbox_grid())
      } else {
        hide("crash-slb")
        hide("crash-slu")
        hide("crash-grid")
        hide("crash-zoom_auto")
        hide("crash-zoom_slider")
      }
    })
    
    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_view_change = reactive(input$map_view_change),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = NS(id, "map"),
      more_args = reactive(c("c-comp_d" = str_extract(bi_time(), "^."),
                             "c-grid" = str_extract(cbox_grid(), "^."),
                             "s-slu" = slider_uni(),
                             "s-slb" = paste(slider_bi(),
                                             collapse = "-")))
    )
    
    # Update click_id() on bookmark
    observeEvent(r$sus_bookmark$active, {
      # Delay of 2000 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      if (isTRUE(r$sus_bookmark$active)) {
        delay(2000, {
          if (!is.null(r$sus_bookmark$select_id)) {
            if (r$sus_bookmark$select_id != "NA") click_id(r$sus_bookmark$select_id)
          }
        })
      }
      
      # So that bookmarking gets triggered only ONCE
      delay(1500, {r$sus_bookmark$active <- FALSE})      
    }, priority = -2)
    
    # Update click_id() on module link
    observeEvent(r$sus_link$activity, {
      # Delay of 2000 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      delay(2000, {
        if (!is.null(r$sus_link$select_id)) click_id(r$sus_link$select_id)
      })
    }, priority = -2)
  })
}
