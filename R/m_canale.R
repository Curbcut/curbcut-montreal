### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  fillPage(fillRow(
    # Sidebar
    fillCol(sidebar_UI(NS(id, "sidebar"), 
                       div(class = "bottom_sidebar", 
                           tagList(legend_UI(NS(id, "legend")),
                                   zoom_UI(NS(id, "zoom"), map_zoom_levels))))),
    # Map
    fillCol(
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      right_panel(id, compare_UI(NS(id, "canale"), make_dropdown()),
                  div(class = "explore_dyk", explore_UI(NS(id, "explore")), 
                      dyk_UI(NS(id, "dyk"))))),
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "canale", 
      var_map = reactive(paste0("left_", df(), "_", "canale_ind_2016")))
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = map_token, zoom = map_zoom, 
              location = map_location)})
    
    # Zoom
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    df <- zoom_server("zoom", zoom = zoom, zoom_levels = map_zoom_levels)
    
    # Time
    time <- reactive("2016")
    
    # Left variable
    var_left <- reactive("canale_ind_2016")
    
    # Compare panel
    var_right <- compare_server(
      id = "canale", 
      var_list = make_dropdown(),
      df = df, 
      time = time)

    # Data
    data <- data_server(
      id = "canale", 
      var_left = var_left,
      var_right = var_right, 
      df = df, 
      zoom = zoom)
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore", 
      x = data, 
      var_left = var_left,
      var_right = var_right, 
      select = reactive(rv_canale$poly_selected),
      zoom = df, 
      build_str_as_DA = TRUE)

    # Legend
    legend_server(
      id = "legend", 
      var_left = var_left, 
      var_right = var_right, 
      zoom_val = df)
    
    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)

    # Update map in response to variable changes or zooming
    observeEvent({
      var_right()
      df()}, map_change(NS(id, "map"), df = data, zoom = df))

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_canale$poly_selected <- NA
      } else rv_canale$poly_selected <- lst$object$properties$id
    })
    
    # Clear poly_selected on zoom
    observeEvent(df(), {rv_canale$poly_selected <- NA},
                 ignoreInit = TRUE)

    # Update map in response to poly_selected change
    observeEvent(rv_canale$poly_selected, {
      if (!is.na(rv_canale$poly_selected)) {
        width <- switch(df(), "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data() %>%
          filter(ID == rv_canale$poly_selected) %>%
          mutate(fill = substr(fill, 1, 7))

        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data_to_add, elevation = 5, fill_colour = "fill", 
            update_view = FALSE, layer_id = "poly_highlight", 
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
        } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "poly_highlight")
        }
      })

    # Clear click status if prompted
    observeEvent(input$`explore-clear_selection`, {rv_canale$poly_selected <- NA})
    
    # data naming for data_export
    data_export <- data_export_server(
      id = "canale",
      df = data, 
      var_left = var_left, 
      var_right = var_right)
    
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
      zoom = df(),
      explore_content = explore_content(),
      poly_selected = rv_canale$poly_selected)})
    
  })
}
