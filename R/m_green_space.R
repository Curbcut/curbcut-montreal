### GREEN SPACE MODULE ########################################################

# UI ----------------------------------------------------------------------

green_space_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Side bar
      sidebar_UI(NS(id, "sidebar"),
                 select_var_UI(NS(id, "left_groupings"), 
                               green_space_groupings,
                               label = i18n$t("Grouping")),
                 select_var_UI(NS(id, "left_type"), green_space_type,
                               label = i18n$t("Type of green space")),
                 div(class = "bottom_sidebar",
                     tagList(legend_UI(NS(id, "legend")),
                             zoom_UI(NS(id, "zoom"), map_zoom_levels))))),
    fillCol(
      
      # Map
      div(class = "mapdeck_div", 
          mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(id, compare_UI(NS(id, "green_space"), make_dropdown()),
                  div(class = "explore_dyk",
                      green_space_explore_UI(NS(id, "explore")), 
                      dyk_UI(NS(id, "dyk"))))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

green_space_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom, map_zoom_levels))
    selection <- reactiveVal(NA)
    
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
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location)})
    
    # Zoom reactive
    observeEvent(input$map_view_change$zoom, {
      zoom(get_zoom(input$map_view_change$zoom, map_zoom_levels))})
    
    # Zoom level for data
    df <- zoom_server(
      id = "zoom", 
      zoom = zoom, 
      zoom_levels = map_zoom_levels)
    
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
      time = reactive("2016"),
      show_panel = choropleth)
    
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
    explore_content <- green_space_explore_server(
      id = "explore",
      x = data,
      var_left = var_left,
      var_right = var_right,
      select = selection,
      df = df,
      standard_info = choropleth)
    
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
    observeEvent(data(),
                 map_change(NS(id, "map"), df = data, zoom = df,
                            overthrow_width = !choropleth()))
    
    # Update poly on click
    observeEvent(input$map_polygon_click, {
      lst <- (jsonlite::fromJSON(input$map_polygon_click))$object$properties$id
      if (is.null(lst)) selection(NA) else selection(lst)
    })
    
    # Clear poly_selected on data change
    observeEvent(data(), selection(NA), ignoreInit = TRUE)
    
    # Update map in response to poly change
    observeEvent(selection(), {
      if (!is.na(selection())) {
        width <- switch(df(), "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data() |> 
          filter(ID == selection()) |> 
          mutate(fill = substr(fill, 1, 7))
        
        mapdeck_update(map_id = NS(id, "map")) |> 
          add_polygon(
            data = data_to_add, elevation = 5, fill_colour = "fill", 
            update_view = FALSE, layer_id = "poly_highlight", 
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      } else {
        mapdeck_update(map_id = NS(id, "map")) |> 
          clear_polygon(layer_id = "poly_highlight")
      }
    })
    
    # Clear click status if prompted
    observeEvent(input$`explore-clear_selection`, selection(NA))
    
  })
}
