### HOUSING MODULE ##############################################################

# UI ----------------------------------------------------------------------

housing_UI <- function(id) {
  tabItem(tabName = "housing",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   select_var_UI(NS(id, "left"), var_list_housing_left)
                   ),
          right_panel(id, 
                      compare_UI(NS(id, "housing"), var_list_housing_right),
                      explore_UI(NS(id, "explore")),
                      dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "housing")))
}

# Server ------------------------------------------------------------------

housing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "housing")
    
    # # Map
    # output$map <- renderMapdeck({
    #   mapdeck(
    #     style = map_style, token = token_housing,
    #     zoom = map_zoom, location = map_location) %>%
    #     add_polygon(data = {borough %>% 
    #         dplyr::select(ID, name, name_2, population, 
    #                       left_var_full = housing_tenant_prop,
    #                       left_var = housing_tenant_prop_q3) %>% 
    #         mutate(group = paste(left_var, "- 1")) %>% 
    #         left_join(colour_borough, by = "group")}, stroke_width = 0, 
    #         stroke_colour = "#FFFFFF", fill_colour = "fill", update_view = FALSE,
    #         id = "ID", auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
    # })
    # 
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_housing,
        zoom = map_zoom, location = map_location) %>%
        add_polygon(data = borough %>%
                      mutate(group = paste(housing_tenant_prop_q3, "- 1")) %>%
                      left_join(colour_borough, by = "group"),
                    stroke_width = 100, stroke_colour = "#FFFFFF", fill_colour = "fill", 
                    update_view = FALSE, id = "ID", auto_highlight = TRUE,
                    highlight_colour = "#FFFFFF90")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_housing$zoom <- case_when(
        input$map_view_change$zoom >= 12 ~ "DA",
        input$map_view_change$zoom >= 10.5 ~ "CT",
        TRUE ~ "borough")
    })
    
    # Left variable server
    var_left_housing <- select_var_server("left", var_list_housing_left)
    
    # String to fetch maps and data
    df <- reactive(rv_housing$zoom)
    #df <- reactive({if (input$grid) "grid" else rv_housing$zoom})
    
    # Right variable server
    var_right_housing <- compare_server("housing", var_list_housing_right, 
                                        df)
    
    # Data
    data_housing <- data_server("housing", var_left_housing,
                                var_right_housing, df,
                                reactive(rv_housing$zoom))
    # # Explore panel
    # explore_server(id = "explore",
    #            x = data_housing,
    #            var_left = var_left_housing,
    #            var_right = var_right_housing,
    #            select = reactive(rv_housing$poly_selected),
    #            zoom = df,
    #            var_left_title = reactive(
    #              names(var_list_housing_left))
    #            #var_left_label = sus_translate(climate_legend)
    #            )
    
    # Explore panel
    explore_server("explore", data_housing, reactive("var_left_housing"),
                   var_right_housing, reactive(rv_housing$poly_selected),
                   reactive(rv_housing$zoom), reactive(names(var_list_housing_left))
                   )

    # Did-you-know panel
    dyk_server("dyk", var_left_housing, var_right_housing)

    # Left map
    small_map_server("left", reactive(paste0("left_", df, "_",
                                             var_left_housing())))

    # Bivariate legend
    legend_bivar_server("housing", var_right_housing)

    # Update map in response to variable changes or zooming
    observeEvent({
      var_left_housing()
      var_right_housing()
      rv_housing$zoom
    }, {

      width <- switch(rv_housing$zoom, "borough" = 100, "CT" = 10, 2)

      mapdeck_update(map_id = NS(id, "map")) %>%
        add_polygon(data = data_housing(), stroke_width = width,
                    stroke_colour = "#FFFFFF", fill_colour = "fill",
                    update_view = FALSE, id = "ID", auto_highlight = TRUE,
                    highlight_colour = "#FFFFFF90")
    }
    )

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_housing$poly_selected <- NA
      } else rv_housing$poly_selected <- lst$object$properties$id
    })

    # Clear poly_selected on zoom
    observeEvent(rv_housing$zoom, {rv_housing$poly_selected <- NA},
                 ignoreInit = TRUE)

    # Update map in response to poly_selected change
    observeEvent(rv_housing$poly_selected, {
      if (!is.na(rv_housing$poly_selected)) {
        width <- switch(rv_housing$zoom, "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data_housing() %>%
          filter(ID == rv_housing$poly_selected) %>%
          mutate(fill = substr(fill, 1, 7))

        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data_to_add, stroke_width = width, stroke_colour = "#000000",
            fill_colour = "fill", update_view = FALSE,
            layer_id = "poly_highlight", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90")
      } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "poly_highlight")
      }
    })

    # Clear click status if prompted
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_housing$poly_selected <- NA})

  })
}
