### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  fillPage(
    fillRow(
      fillCol(sidebar_UI(NS(id, "sidebar"),
                         div(class = "bottom_sidebar",
                             tagList(legend_UI(NS(id, "legend")),
                                     zoom_UI(NS(id, "zoom"), canale_zoom)))
      )),
      fillCol(
        div(class = "mapdeck_div", 
            mapdeckOutput(NS(id, "map"), height = "100%")),
        right_panel(id, compare_UI(NS(id, "canale"), var_list_canale),
                    explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk")))),
      flex = c(1, 5)
    )
  )
}


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Sidebar
    sidebar_server("sidebar", "canale", 
                   reactive(paste0("left_", zoom(), "_", canale_ind)))
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = map_token, zoom = map_zoom, 
              location = map_location) |>
        add_sf(data = 
                 borough |> 
                 mutate(group = as.character(canale_ind_q3_2016),
                        group = if_else(is.na(group), "NA", group)) |> 
                 left_join(colour_left_3_borough, by = "group"),
               stroke_width = 100, stroke_colour = "#FFFFFF", 
               fill_colour = "fill", update_view = FALSE, id = "ID", 
               auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      })


    # Zoom
    zoom_val <- reactiveVal(get_zoom(map_zoom, canale_zoom))
    observeEvent(input$map_view_change$zoom, {
      zoom_val(get_zoom(input$map_view_change$zoom, canale_zoom))
    })
    zoom <- zoom_server("zoom", zoom = zoom_val, zoom_levels = canale_zoom)
    
    # Left variable
    var_left <- reactive(canale_ind)
    
    # Compare panel
    var_right <- compare_server(id = "canale", var_list = var_list_canale,
                                df = zoom)

    # Data
    data <- data_server(id = "canale", var_left = var_left,
                        var_right = var_right, df = zoom, zoom = zoom_val)
    
    # Explore panel
    explore_server(id = "explore", 
                   x = data, 
                   var_left = var_left,
                   var_right = var_right, 
                   select = reactive(rv_canale$poly_selected),
                   zoom = zoom, 
                   build_str_as_DA = TRUE)

    # Legend
    legend_server("legend", var_left, var_right, zoom_val)
    
    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)

    # Update map in response to variable changes or zooming
    observeEvent({
      var_right()
      zoom()}, map_change(NS(id, "map"), df = data, zoom = zoom))

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_canale$poly_selected <- NA
      } else rv_canale$poly_selected <- lst$object$properties$id
    })
    
    # Clear poly_selected on zoom
    observeEvent(zoom(), {rv_canale$poly_selected <- NA},
                 ignoreInit = TRUE)

    # Update map in response to poly_selected change
    observeEvent(rv_canale$poly_selected, {
      if (!is.na(rv_canale$poly_selected)) {
        width <- switch(zoom(), "borough" = 100, "CT" = 10, 2)
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
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_canale$poly_selected <- NA})
    
  })
}
