#### RDECK TEST ################################################################

library(rdeck)

shinyApp(
  
  ui = fillPage(
    rdeckOutput("map", height = "100%"),
    absolutePanel(top = 10, left = 10, 
                  selectInput("choose", label = "Variable", choices = list(
                    canale = "canale_ind_q5_2016",
                    housing = "housing_tenant_pct_q5_2016")))),
  
  server = function(input, output) {
    
    output$map <- renderRdeck({
      rdeck(initial_view_state = view_state(center = c(-73.58, 45.53), 
                                            zoom = 9)) |> 
        add_mvt_layer(id = "canale",
                      data = mvt_url("dwachsmuth.borough_5"),
                      auto_highlight = TRUE,
                      pickable = TRUE,
                      tooltip = TRUE,
                      get_fill_color = scale_color_category(
                        col = "canale_ind_q5_2016",
                        palette = colour_left_5$fill[1:6],
                        unmapped_color = colour_left_5$fill[1],
                        levels = 0:5),
                      get_line_color = "#FFFFFF")
      
    })
    
    observe({
      
      rdeck_proxy("map") |>
        add_mvt_layer(id = "canale",
                      auto_highlight = TRUE,
                      pickable = TRUE,
                      tooltip = TRUE,
                      get_fill_color = 
                        scale_color_category(
                          col = !!rlang::sym(input$choose),
                          palette = colour_left_5$fill[1:6],
                          unmapped_color = colour_left_5$fill[1],
                          levels = 0:5),
                      get_line_color = "#FFFFFF")
    })
  }
)
