#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  fluidPage(plotOutput(NS(id, "legend")))
}

legend_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
   
    renderPlot({
      ggplot(borough) +
        geom_sf()
      # col_bivar |> 
      #   ggplot(aes(x, y, fill = fill)) +
      #   geom_tile() +
      #   geom_text(aes(x, y, label = label), inherit.aes = FALSE) +
      #   scale_fill_manual(values = setNames(colour_borough$fill[1:9],
      #                                       colour_borough$fill[1:9])) +
      #   labs(x = "Canale (low to high)", y = "Housing (low to high)") +
      #   theme_void() +
      #   theme(legend.position = "none")
    }, width = "100px") 
  })
}
