#### HOUSING MODULE ############################################################

# UI ----------------------------------------------------------------------

housing_UI <- function(id) {
  ns_id <- "housing"
  
  return(tagList(
      # Sidebar
      sidebar_UI(
        NS(id, ns_id),
        susSidebarWidgets(
          select_var_UI(NS(id, ns_id), var_list = vars_housing_left), 
          slider_UI(NS(id, ns_id), slider_id = "slu"), 
          slider_UI(NS(id, ns_id), slider_id = "slb",
                    label = sus_translate("Select two years"),
                    value = c("2006", "2016")), 
          checkbox_UI(NS(id, ns_id),
            label = sus_translate("Compare dates")),
          year_disclaimer_UI(NS(id, ns_id))
        ),
        bottom = div(class = "bottom_sidebar", 
            tagList(legend_UI(NS(id, ns_id)),
                    zoom_UI(NS(id, ns_id), map_zoom_levels)))),
    
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id,
        compare_UI(NS(id, ns_id), vars_housing_right),
        explore_UI(NS(id, ns_id)), 
        dyk_UI(NS(id, ns_id)))
  ))
}


# Server ------------------------------------------------------------------

housing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "housing"
    
    # Initial zoom reactive
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    click_id <- reactiveVal(NULL)

    # Map
    output$map <- renderMapdeck(mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location))
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Click reactive
    observeEvent(input$map_polygon_click, {
      click_id(get_click(input$map_polygon_click))})
    
    # Zoom level for data
    df <- zoom_server(
      id = ns_id, 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Checkbox value
    slider_switch <- checkbox_server(id = ns_id)
    
    # Enable or disable first and second slider
    observeEvent(slider_switch(), {
      toggle(NS(id, "slu"), condition = !slider_switch())
      toggle(NS(id, "slb"), condition = slider_switch())
    })
    
    # Time variable depending on which slider is active
    slider_uni <- slider_server(id = ns_id, slider_id = "slu")
    slider_bi <- slider_server(id = ns_id, slider_id = "slb")
    time <- reactive({
      if (slider_switch()) slider_bi() else slider_uni()})
    
    # Left variable server
    var_left <- select_var_server(
      id = ns_id, 
      var_list = reactive(vars_housing_left), 
      disabled = reactive(if (!slider_switch()) NULL else 
        vars_housing_left_dis),
      time = time, 
      df = df)

    # Right variable/compare panel
    var_right <- compare_server(
      id = ns_id, 
      var_list = vars_housing_right, 
      disabled = reactive(if (!slider_switch()) NULL else 
        vars_housing_right_dis),
      df = df,
      time = time)

    # Sidebar
    sidebar_server(id = ns_id, x = "housing")
    
    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right()))
    
    # Legend
    legend_server(
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
    
    # Year disclaimer
    year_disclaimer_server(
      id = ns_id, 
      data = data,
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
    
    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_view_change = reactive(input$map_view_change),
      var_left = var_left,
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = NS(id, "map"),
      more_args = reactive(c("c-cbox" = str_extract(slider_switch(), "^."),
                             "s-slu" = slider_uni(),
                             "s-slb" = paste(slider_bi(),
                                              collapse = "-")))
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
