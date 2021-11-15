### ACCESS MODULE ##############################################################

# UI ----------------------------------------------------------------------

access_UI <- function(id) {
  tabItem(tabName = "access",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   select_var_UI(NS(id, "left_2"), var_list_left_access_2,
                                 label = i18n$t("Timing"), width = "200px"),
                   select_var_UI(NS(id, "left_1"), var_list_left_access_1,
                        label = i18n$t("Destination type"), width = "200px"),
                   div(style = widget_style, 
                       sliderInput(NS(id, "slider"), i18n$t("Time threshold"),
                                   min = 10, max = 60, step = 1, value = 30,
                                   width = "200px")),
                   shinyjs::useShinyjs()),
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
               auto_highlight = TRUE, highlight_colour = "#FFFFFF90"#,
               # palette = access_colour(c(0, 0.2, 0.4, 0.6, 0.8, 1))
               )
      })
    
    # Enable or disable inputs
    observeEvent(rv_access$poly_selected, {
      shinyjs::toggle("left_1-var", condition = is.na(rv_access$poly_selected))
      shinyjs::toggle("slider", condition = !is.na(rv_access$poly_selected))
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
      }, map_change(NS(id, "map"), df = data, zoom = reactive("CT")))

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      click <- jsonlite::fromJSON(input$map_polygon_click)$object$properties$id
      print(click)
      print(rv_access$poly_selected)
      if (is.null(click)) {
        rv_access$poly_selected <- NA
      } else if (!is.na(rv_access$poly_selected) && 
                 click == rv_access$poly_selected) {
        rv_access$poly_selected <- NA
      } else rv_access$poly_selected <- click
    })
    
    # Update map in response to poly_selected change
    observeEvent({
      rv_access$poly_selected
      var_left_2()
      input$slider}, {
      if (!is.na(rv_access$poly_selected)) {
        
        tt_thresh <- input$slider * 60
        
        CTs_to_map <- 
          tt_matrix |> 
          filter(origin == rv_access$poly_selected, travel_time <= tt_thresh,
                 timing == var_left_2()) |> 
          mutate(group = 4 - pmax(1, ceiling(travel_time / tt_thresh * 3))) |> 
          select(destination, group) |> 
          left_join(colour_isopleth, by = "group")
        
        data_to_add <-
          data() %>%
          select(ID) |> 
          inner_join(CTs_to_map, by = c("ID" = "destination"))
        
        poly_to_add <-
          data() %>%
          filter(ID == rv_access$poly_selected) %>%
          mutate(fill = "#00000033")
        
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon() %>%
          add_polygon(
            data = CT, stroke_width = 10, stroke_colour = "#FFFFFF", id = "ID",
            fill_colour = "#FFFFFF10", update_view = FALSE,
            layer_id = "poly_bg", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90") %>%
          add_polygon(
            data = data_to_add, stroke_width = 10, stroke_colour = "#FFFFFF",
            fill_colour = "fill", update_view = FALSE, id = "ID",
            layer_id = "poly_iso", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90") %>%
          add_polygon(
            data = poly_to_add, fill_colour = "fill", stroke_width = 20,
            stroke_colour = "#000000", update_view = FALSE, id = "ID",
            layer_id = "poly_highlight", auto_highlight = TRUE, 
            highlight_colour = "#FFFFFF90")
        } else {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "poly_bg") %>%
            clear_polygon(layer_id = "poly_iso") %>%
            clear_polygon(layer_id = "poly_highlight")
        }
      })

    # Clear click status if prompted
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_access$poly_selected <- NA})
    
  })
}
