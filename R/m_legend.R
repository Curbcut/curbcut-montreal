#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  tagList(
    h5("Legend"),
    plotOutput(NS(id, "legend"), height = "170px")
  )
}

legend_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
   
    col_bivar <- 
      colour_borough |> 
      slice(1:9) |> 
      tidyr::separate(group, into = c("x", "y"), sep = " - ") |> 
      mutate(label = case_when(
        x == "3" & y == "3" ~ "Both high",
        x == "1" & y == "1" ~ "Both low",
        x == "3" & y == "1" ~ "Canale\nhigh only",
        x == "1" & y == "3" ~ "Housing\nhigh only",
        TRUE ~ NA_character_)) |> 
      mutate(label_colour = if_else(label == "Both high", "white", "black"))
    
    output$legend <- renderPlot({
      col_bivar |>
        ggplot(aes(x, y, fill = fill)) +
        geom_tile() +
        geom_text(aes(x, y, label = label, colour = label_colour), 
                  inherit.aes = FALSE, size = 4.5) +
        scale_fill_manual(values = setNames(colour_borough$fill[1:9],
                                            colour_borough$fill[1:9])) +
        scale_colour_manual(values = c("black" = "black", "white" = "white")) +
        labs(x = "Canale (low to high)", y = "Housing (low to high)") +
        theme_void() +
        theme(legend.position = "none")
    }, height = 170) 
  })
}
