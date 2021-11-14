### ACCESS MODULE ##############################################################

# UI ----------------------------------------------------------------------

access_UI <- function(id) {
  tabItem(tabName = "access",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   div(style = "display: inline-block; padding: 5px;", 
                       select_var_UI(NS(id, "left_1"), var_list_left_access_1,
                        label = i18n$t("Destination type"), 
                        width = "200px")),
                   div(style = "display: inline-block; padding: 5px;", 
                       select_var_UI(NS(id, "left_2"), var_list_left_access_2,
                        label = i18n$t("Timing"),
                        width = "200px"))),
          right_panel(id, compare_UI(NS(id, "access"), var_list_right_access),
                      explore_UI(NS(id, "explore")), dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "access")))
  }


# Server ------------------------------------------------------------------

access_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "access")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_access, zoom = map_zoom, 
              location = map_location) %>%
        add_sf(data = 
                 CT %>%
                 mutate(group = paste0(eval(as.name(
                   "access_jobs_total_pwd_q3")), " - 1")) %>%
                 left_join(colour_CT, by = "group"),
               stroke_width = 10, stroke_colour = "#FFFFFF", 
               fill_colour = "fill", update_view = FALSE, id = "ID", 
               auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      })
    
    # Left variable servers
    var_left_1 <- select_var_server("left_1", reactive(var_list_left_access_1))
    var_left_2 <- select_var_server("left_2", reactive(var_list_left_access_2))
    
    # Construct left variable string
    var_left <- reactive(paste0(var_left_1(), "_", var_left_2()))
    
    # Compare panel
    var_right <- compare_server(id = "access", var_list = var_list_right_access,
                                df = reactive("CT"))

    # Data
    data <- data_server(id = "access", var_left = var_left,
                        var_right = var_right, df = reactive("CT"))
    
    # Explore panel
    explore_server(id = "explore", 
                   x = data, 
                   var_left = var_left,
                   var_right = var_right, 
                   select = reactive(rv_access$poly_selected),
                   zoom = reactive("CT"), 
                   build_str_as_DA = TRUE)

    # Did-you-know panel
    dyk_server("dyk", var_left, var_right)

    # Left map
    small_map_server("left", reactive(paste0("left_CT_", var_left())))
    
    # Bivariate legend
    legend_bivar_server("access", var_right)
    
    # Update map in response to variable changes
    observeEvent({
      var_left()
      var_right()
      }, map_change(NS(id, "map"), df = data, 
                                  zoom = reactive("CT")))

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_access$poly_selected <- NA
      } else rv_access$poly_selected <- lst$object$properties$id
    })
    
    # Update map in response to poly_selected change
    observeEvent(rv_access$poly_selected, {
      if (!is.na(rv_access$poly_selected)) {
        
        width <- 10
        tt_threshold <- 1800
        
        CTs_to_map <- 
          tt_12_weekend |> 
          filter(origin == rv_access$poly_selected, travel_time <= tt_threshold) |> 
          mutate(tt_q3 = ntile(travel_time, 3)) |> 
          select(destination, tt_q3) |> 
          mutate(destination = as.character(destination)) |> 
          mutate(group = paste0(tt_q3, " - 1")) |> 
          left_join(colour_CT, by = "group")
        
        print("CTs_to_map")
        print(CTs_to_map)
        print("data")
        print(data())
        
        
        data_to_add <-
          data() %>%
          select(ID) |> 
          inner_join(CTs_to_map, by = c("ID" = "destination")) |> 
          mutate(fill = substr(fill, 1, 7))

        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon() %>%
          add_polygon(
            data = data_to_add, stroke_width = 0.5, stroke_colour = "#FFFFFF",
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
      rv_access$poly_selected <- NA})
    
  })
}
