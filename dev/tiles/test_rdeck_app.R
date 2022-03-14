#### RDECK TEST APP ############################################################

shinyApp(
  ui = fillPage(
    rdeckOutput("map", height = "100%"),
    absolutePanel(top = 10, left = 10, 
                  selectInput("fill", label = "Variable", choices = list(
                    " ", "housing_tenant_pct_2016"))
    )
  ),
  
  server = function(input, output) {
    
    output$map <- renderRdeck(
      rdeck(theme = map_style,
            initial_view_state = view_state(center = c(-73.58, 45.53), 
                                            zoom = 10.1)) |> 
        add_mvt_layer(id = "test", 
                      data = mvt_url("dwachsmuth.canale-borough3"),
                      auto_highlight = TRUE, highlight_color = "#AAFFFFFF",
                      pickable = TRUE, tooltip = TRUE, 
                      get_fill_color = scale_color_category(
                        col = canale_ind_2016, 
                        palette = paste0(c(colour_left_5$fill, 
                                           colour_bivar$fill), "EE"),
                        unmapped_color = paste0(colour_left_5$fill[1], "EE"),
                        levels = c(paste0("q5_", colour_left_5$group), 
                                   colour_bivar$group)),
                      get_line_color = "#FFFFFF")
    )
    
    var_right <- reactive(input$fill)
    
    observe({
      rdeck_proxy("map") |>
        add_mvt_layer(id = "test",
                          auto_highlight = TRUE, highlight_color = "#AAFFFFFF",
                          pickable = TRUE, tooltip = TRUE,
                          get_fill_color = scale_color_category(
                            col = !!rlang::sym(
                              str_remove(paste("canale_ind_2016", var_right(), 
                                               sep = "_"), "_ $")), 
                            palette = paste0(c(colour_left_5$fill, 
                                             colour_bivar$fill), "EE"),
                          unmapped_color = paste0(colour_left_5$fill[1], "EE"),
                          levels = c(paste0("q5_", colour_left_5$group), 
                                     colour_bivar$group)),
                          get_line_color = "#FFFFFF")
    })
  }
)
