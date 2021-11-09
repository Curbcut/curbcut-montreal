### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  tabItem(tabName = "canale",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title")),
          right_panel(id, compare_UI(NS(id, "canale"), var_list_canale),
                      explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "canale")))
  }


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "canale")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_canale, zoom = map_zoom, 
              location = map_location) %>%
        add_sf(data = 
                 borough %>%
                 mutate(group = paste(eval(as.name(paste0(
                   "canale_ind_q3", "_", current_census))), "- 1")) %>%
                 left_join(colour_borough, by = "group"),
               stroke_width = 100, stroke_colour = "#FFFFFF", 
               fill_colour = "fill", update_view = FALSE, id = "ID", 
               auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_canale$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "building",
                                  input$map_view_change$zoom >= 12 ~ "DA",
                                  input$map_view_change$zoom >= 10.5 ~ "CT",
                                  TRUE ~ "borough")})
        
    # Left variable
    var_left <- reactive(canale_ind)
    
    # Compare panel
    var_right <- compare_server(id = "canale", var_list = var_list_canale,
                                df = reactive(rv_canale$zoom))

    # Data
    data <- data_server(id = "canale", var_left = var_left,
                        var_right = var_right, df = reactive(rv_canale$zoom))
    
    # Explore panel
    explore_server(id = "explore", 
                   x = data, 
                   var_left = var_left,
                   var_right = var_right, 
                   select = reactive(rv_canale$poly_selected),
                   zoom = reactive(rv_canale$zoom), 
                   build_str_as_DA = TRUE)

    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)

    # Left map
    small_map_server("left", reactive(paste0(
      "left_", rv_canale$zoom, "_", canale_ind)))
    
    # Bivariate legend
    legend_bivar_server("canale", var_right)
    
    # Update map in response to variable changes or zooming
    observeEvent({
      var_right()
      rv_canale$zoom}, map_change(NS(id, "map"), df = data, 
                                  zoom = reactive(rv_canale$zoom)))

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_canale$poly_selected <- NA
      } else rv_canale$poly_selected <- lst$object$properties$id
    })
    
    # Clear poly_selected on zoom
    observeEvent(rv_canale$zoom, {rv_canale$poly_selected <- NA},
                 ignoreInit = TRUE)

    # Update map in response to poly_selected change
    observeEvent(rv_canale$poly_selected, {
      if (!is.na(rv_canale$poly_selected)) {
        width <- switch(rv_canale$zoom, "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data() %>%
          filter(ID == rv_canale$poly_selected) %>%
          mutate(fill = substr(fill, 1, 7))

        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            # data = data_to_add, stroke_width = width, stroke_colour = "#000000",
            data = data_to_add, #stroke_width = 0.5, stroke_colour = "#FFFFFF",
            elevation = 5,
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
      rv_canale$poly_selected <- NA})
    
  })
}
