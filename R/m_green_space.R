### GREEN SPACE MODULE ########################################################

# UI ----------------------------------------------------------------------

green_space_UI <- function(id) {
  fillPage(
    fillRow(
      fillCol(sidebar_UI(NS(id, "sidebar"),
                         select_var_UI(NS(id, "left_groupings"), 
                                       green_space_groupings,
                                       label = i18n$t("Grouping")),
                         select_var_UI(NS(id, "left_type"), green_space_type,
                                       label = i18n$t("Type of green space")),
                         div(class = "bottom_sidebar",
                             tagList(legend_UI(NS(id, "legend")),
                                     zoom_UI(NS(id, "zoom"), map_zoom_levels)))
      )),
      fillCol(
        div(class = "mapdeck_div", 
            mapdeckOutput(NS(id, "map"), height = "100%")),
        right_panel(id, compare_UI(NS(id, "green_space"), make_dropdown()),
                    div(class = "explore_dyk",
                        explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))))),
      flex = c(1, 5)
    )
  )
}


# Server ------------------------------------------------------------------

green_space_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "green_space"#, 
      # Adding a small map for the green_space df is needed
      # var_map = reactive(paste0("left_", df(), "_", "green_space"))
      )
    
    # If green space isn't selected, choropleth is TRUE 
    choropleth <- reactive(var_left_groupings() != " ")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = map_token, zoom = map_zoom, 
              location = map_location)
    })
    
    # Zoom
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    df <- zoom_server("zoom", zoom = zoom, zoom_levels = map_zoom_levels)
    
    # Left variable servers
    var_left_groupings <- select_var_server("left_groupings", 
                                    reactive(green_space_groupings))
    var_left_type <- select_var_server("left_type", 
                                    reactive(green_space_type))
    
    # Construct left variable string
    var_left <- reactive(str_remove(paste("green_space", var_left_type(), 
                                          var_left_groupings(), sep = "_"), 
                                    "_ "))
    
    # Compare panel
    var_right <- compare_server(
      id = "green_space", 
      var_list = make_dropdown(),
      df = df,
      time = reactive("2016"))
    
    # Data
    data_choropleth <- data_server(
      id = "green_space", 
      var_left = var_left,
      var_right = var_right, 
      df = df, 
      zoom = zoom)
    
    data <- reactive({
      if (choropleth()) {
        data_choropleth() %>% 
          {if (nrow(.) == nrow(borough))
            filter(., ID %in% island_csduid)
            else filter(., CSDUID %in% island_csduid)}
      } else {
        green_space %>%
          {if (var_left_type() != "total")
            filter(., type_1 == var_left_type()) else .}
      }
    })
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore",
      x = data,
      var_left = var_left,
      var_right = var_right,
      select = reactive(rv_green_space$poly_selected),
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
    observeEvent(data(),
                 map_change(NS(id, "map"), df = data, zoom = df,
                            overthrow_width = !choropleth()))
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_green_space$poly_selected <- NA
      } else rv_green_space$poly_selected <- lst$object$properties$id
    })
    
    # Clear poly_selected on zoom
    observeEvent({df()
      var_left()}, {rv_green_space$poly_selected <- NA},
      ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observeEvent(rv_green_space$poly_selected, {
      if (!is.na(rv_green_space$poly_selected)) {
        width <- switch(df(), "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data() %>%
          filter(ID == rv_green_space$poly_selected) %>%
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
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_green_space$poly_selected <- NA})

    
  })
}
