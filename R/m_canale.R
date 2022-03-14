### CANALE MODULE ##############################################################

sus_scale <- function(var) scale_color_category(
  col = !!var, palette = paste0(c(colour_left_5$fill, colour_bivar$fill), "EE"),
  unmapped_color = paste0(colour_left_5$fill[1], "EE"), 
  levels = c(paste0("q5_", colour_left_5$group), colour_bivar$group))


# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  
  ns_id <- "canale"
  
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, ns_id), 
      bottom = div(class = "bottom_sidebar", 
                   tagList(legend_UI(NS(id, ns_id)),
                           zoom_UI(NS(id, ns_id), map_zoom_levels)))),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, "map"), height = "100%")),
    
    # Right panel
    right_panel(
      id = id,
      compare_UI(NS(id, ns_id), make_dropdown()),
      explore_UI(NS(id, ns_id)), 
      dyk_UI(NS(id, ns_id)))
    
    )
}


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "canale"
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    click_id <- reactiveVal(NULL)
    poi <- reactiveVal(NULL)
    
    # Map
    output$map <- renderRdeck({
      rdeck(theme = map_style, initial_view_state = view_state(
        center = c(-73.58, 45.53), zoom = 10.1)) |> 
        add_mvt_layer(id = "canale", 
                      data = mvt_url("dwachsmuth.canale-autozoom-test-1"),
                      auto_highlight = TRUE, highlight_color = "#AAFFFFFF",
                      pickable = TRUE, tooltip = TRUE,
                      get_fill_color = sus_scale("canale_ind_2016"),
                      get_line_color = "#FFFFFF")
        })
    
    # # Zoom and POI reactives
    # observeEvent(input$map_view_change, {
    #   zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))
    #   poi(observe_map(input$map_view_change))
    # })
    # 
    # # Click reactive
    # observeEvent(input$map_polygon_click, {
    #   click_id(get_click(input$map_polygon_click))})
    
    # Zoom level for data
    df <- zoom_server(
      id = ns_id, 
      zoom = zoom, 
      zoom_levels = reactive(map_zoom_levels))
    
    # Time
    time <- reactive("2016")
    
    # Left variable
    var_left <- reactive(paste0("canale_ind_", time()))
    
    # Right variable / compare panel
    var_right <- compare_server(
      id = ns_id, 
      var_list = make_dropdown(),
      df = df, 
      time = time)
    
    map_var <- reactive(
      str_remove(paste(var_left(), var_right(), sep = "_"), "_ $"))
    
    # Sidebar
    sidebar_server(id = ns_id, x = "canale")
    
    # Data
    data <- reactive(get_data(
      df = df(), 
      var_left = var_left(), 
      var_right = var_right()))
    
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
      var_right = var_right,
      poi = poi)
    
    # Update map in response to variable changes or zooming
    select_id <- reactive(NA_character_)
    # select_id <- map_change(
    #   id = ns_id,
    #   map_id = NS(id, "map"), 
    #   data = data, 
    #   df = df, 
    #   zoom = zoom,
    #   click = click_id
    #   )
    
    # Observe variable changes to update map
    observe({
      rdeck_proxy("map") |>
        add_mvt_layer(id = "canale", auto_highlight = TRUE,
                      highlight_color = "#AAFFFFFF", pickable = TRUE,
                      get_fill_color = sus_scale(rlang::sym(map_var())),
                      get_line_color = "#FFFFFF")
    })
    
    # Explore panel
    explore_content <- explore_server(
      id = ns_id, 
      data = data, 
      var_left = var_left,
      var_right = var_right, 
      df = df, 
      zoom = zoom,
      select_id = select_id)
    
    # Data export TKTK should this become a non-reactive function?
    data_export <- data_export_server(
      id = ns_id,
      df = data,
      var_left = var_left,
      var_right = var_right)
    
    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_view_change = reactive(input$map_view_change),
      var_right = var_right,
      select_id = select_id,
      df = df,
      map_id = NS(id, "map"),
    )
    
    # Update click_id() on bookmark
    observeEvent(sus_bookmark$active, {
      # Delay of 2000 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      if (isTRUE(sus_bookmark$active)) {
        delay(2000, {
          if (!is.null(sus_bookmark$select_id)) {
            if (sus_bookmark$select_id != "NA") click_id(sus_bookmark$select_id)
          }
        })
      }
      
      # So that bookmarking gets triggered only ONCE
      delay(1500, {sus_bookmark$active <- FALSE})      
    }, priority = -2)
    
    # Update click_id() on module link
    observeEvent(sus_link$activity, {
      # Delay of 2000 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      delay(2000, {
        if (!is.null(sus_link$select_id)) click_id(sus_link$select_id)
      })
    }, priority = -2)
    
    # OUT
    reactive({list(
      module_short_title = "the CanALE index",
      module_id = "canale",
      time = "2016",
      data = data_export(),
      token = map_token,
      map_zoom = input$map_view_change$zoom,
      map_location = c(input$map_view_change$longitude, 
                       input$map_view_change$latitude),
      df = df(),
      explore_content = explore_content(),
      poly_selected = selection())})
    
  })
}
