### CLIMATE RISK MODULE ########################################################

# UI ----------------------------------------------------------------------

climate_risk_UI <- function(id) {
  ns_id <- "climate_risk"
  
  return(tagList(
      # Sidebar
      sidebar_UI2(
        NS(id, ns_id),
        susSidebarWidgets(
          select_var_UI(NS(id, ns_id), var_list = var_list_climate_risk), 
          checkbox_UI(NS(id, ns_id), value = TRUE,
                      label = sus_translate("250-metre grid")),
        ),
        bottom = div(class = "bottom_sidebar",
            tagList(legend_UI(NS(id, ns_id)),
                    zoom_UI(NS(id, ns_id), map_zoom_levels)))),

      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id, 
        compare_UI(NS(id, ns_id), make_dropdown()),
        explore_UI(NS(id, ns_id)), 
        dyk_UI(NS(id, ns_id)))
  ))
}


# Server ------------------------------------------------------------------

climate_risk_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "climate_risk"
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    click_id <- reactiveVal(NULL)
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)})
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Click reactive
    observeEvent(input$map_polygon_click, {
      click_id(get_click(input$map_polygon_click))})
    
    # Zoom level for data
    df_choropleth <- zoom_server(
      id = ns_id, 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Grid value
    grid <- checkbox_server(id = ns_id)
    
    # String to fetch maps and data
    df <- reactive(if (grid()) "grid" else df_choropleth())
    
    # Time
    time <- reactive("2016")
    
    # Left variable server
    var_left <- select_var_server(ns_id, var_list = reactive(var_list_climate_risk))
    
    # Right variable/compare panel
    var_right <- compare_server(
      id = ns_id, 
      var_list = make_dropdown(),
      df = df, 
      time = time)
    
    # Sidebar
    sidebar_server(id = ns_id, x = "climate_risk")
    
    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right(), 
      island = TRUE))
    
    # Legend
    legend <- legend_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      zoom = zoom)

    # Did-you-know panel
    dyk_server(
      id = ns_id,
      var_left = var_left,
      var_right = var_right)
    
    # Update map in response to variable changes or zooming
    select_id <- map_change(
      id = ns_id,
      map_id = NS(id, "map"),
      data = data,
      df = df,
      zoom = zoom,
      click = click_id,
    )
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id,
      data = data,
      var_left = var_left,
      var_right = var_right,
      df = df,
      zoom = zoom,
      select_id = select_id)
    
    # If grid isn't clicked, toggle on the zoom menu
    observeEvent(grid(), {
      toggle("climate_risk-zoom_auto", condition = !grid())
      toggle("climate_risk-zoom_slider", condition = !grid())
    })
    
    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_view_change = reactive(input$map_view_change),
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = NS(id, "map"),
      more_args = reactive(c("c-cbox" = as.logical(grid()),
                             "d-var" = get_variables_rowid(input$`climate_risk-var`)))
    )

    # Last bookmark step: update click_id() + mark bookmark as inactive
    observeEvent(sus_bookmark$active, {
      # Delay of 100 milliseconds more than the map update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      if (isTRUE(sus_bookmark$active)) {
        delay(1100, {
          if (!is.null(sus_bookmark$select_id)) {
            if (sus_bookmark$select_id != "NA") click_id(sus_bookmark$select_id)
          }
        })
      }

      # So that bookmarking gets triggered only ONCE
      delay(1500, {sus_bookmark$active <- FALSE})

    }, priority = -2)
  })
}
