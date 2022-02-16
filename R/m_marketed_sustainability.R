### MARKETED SUSTAINABILITY MODULE #############################################################

# UI ----------------------------------------------------------------------

marketed_sustainability_UI <- function(id) {
  return(tagList(
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
      div(class = "bottom_sidebar", 
          h5("Legend", style = "font-size: 12px;"),
          plotOutput(NS(id, "legend"), height = 60))),
    
      # Map
      div(class = "mapdeck_div", 
          mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Right panel
      right_panel(
        id = id, 
        # compare_UI(NS(id, "marketed_sustainability"), make_dropdown()),
        div(class = "explore_dyk", 
            explore_UI(NS(id, "explore")), 
            dyk_UI(NS(id, "dyk"))))
  ))
}


# Server ------------------------------------------------------------------

marketed_sustainability_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    selection <- reactiveVal(NA)

    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "marketed_sustainability", 
      var_map = NULL)
    
    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location) |>
        add_scatterplot(data = marketed_sustainability, update_view = FALSE,
                        id = "ID",
                        fill_colour = "fill",
                        stroke_colour = marketed_sustainability |> 
                          filter(sustainability_prop == max(sustainability_prop)) |> 
                          pull(fill) |> 
                          unique(),
                        auto_highlight = TRUE,
                        highlight_colour = "#FFFFFF90",
                        radius = 12,
                        radius_min_pixels = 10)})
    
    # Legend
      output$legend <- renderPlot({
        n <- 1000
        leg <- tibble(x = max(marketed_sustainability$sustainability_prop)/n*(1:n),
                      val = max(marketed_sustainability$sustainability_prop)/n*(1:n),
                      type = 1)
        
        min_col <- marketed_sustainability |> 
          filter(sustainability_prop == min(sustainability_prop)) |> 
          pull(fill) |> 
          unique()
        
        mid_col <- marketed_sustainability |> 
          filter(sustainability_prop == median(sustainability_prop)) |> 
          pull(fill) |> 
          unique()
        
        max_col <- marketed_sustainability |> 
          filter(sustainability_prop == max(sustainability_prop)) |> 
          pull(fill) |> 
          unique()
        
        ggplot(leg, aes(x = x, y = 1)) +
          geom_col(aes(fill = x)) +
          scale_fill_gradient2(low = min_col, mid = mid_col, high = max_col, 
                               midpoint = median(leg$x)) +
          scale_x_continuous(name = "Prop. of sustainability-related words",
                             labels = scales::percent) +
          scale_y_continuous(name = NULL, labels = NULL) +
          theme_minimal() +
          theme(legend.position = "none",
                panel.grid = element_blank())
      })
    
    # Explore panel
    explore_content <- explore_server(
      id = "explore",
      data = reactive(marketed_sustainability),
      var_left = reactive(NULL),
      var_right = reactive(NULL),
      df = reactive(NULL),
      select_id = selection,
      standard = reactive(FALSE),
      custom_info = marketed_sustainability_info_table,
      custom_graph = marketed_sustainability_explore_graph)
    
    # Did-you-know panel
    dyk_server(
      id = "dyk",
      var_left = reactive(NULL),
      var_right = reactive(NULL))
    
    # Update select_id on click
    observeEvent(input$map_scatterplot_click, {
      lst <- fromJSON(input$map_scatterplot_click)$index
      if (is.null(lst)) selection(NA) else {
        # This is a hack because of a mapdeck bug
        selection(marketed_sustainability[lst + 1,]$ID)
      }
    })
    
    # Update map on click
    observeEvent(selection(), {
      if (!is.na(selection())) {
        data_to_add <-
          marketed_sustainability |>
          filter(ID == selection())
        
        mapdeck_update(map_id = NS(id, "map")) |>
          add_scatterplot(
            data = data_to_add, update_view = FALSE,
            layer_id = "selected",
            id = "ID",
            fill_colour = "fill",
            stroke_colour = "#000000",
            auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90",
            radius = 250,
            radius_min_pixels = 25)
      } else {
        mapdeck_update(map_id = NS(id, "map")) |>
          clear_scatterplot(layer_id = "selected")
      }
    })

    
    # Clear click status if prompted
    observeEvent(input$`explore-clear_selection`, selection(NA))

  })
}
