#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  div(h5("Legend", style = "font-size: 11px;"),
      plotOutput(NS(id, "legend"), inline = TRUE))
}

legend_server <- function(id, var_left, var_right, zoom_val) {
  
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(zoom_val))
  
  moduleServer(id, function(input, output, session) {
    
    output$legend <- renderPlot({
      
      if (var_right() == " ") {
        
        # Temporarily use legend_left_3, and sub in legend_left_5 once
        # absolute values are showing
        legend_left_3 |> 
          ggplot(aes(x, y, fill = fill, label = label)) +
          geom_tile() +
          geom_text(aes(x, y, label = label), inherit.aes = FALSE, size = 3) +
          scale_fill_manual(values = setNames(
            paste0(legend_left_3$fill, 
                   filter(colour_alpha, zoom == zoom_val())$alpha),
            legend_left_3$fill)) +
          theme_void() +
          theme(legend.position = "none")
        
      } else {
        col_bivar |>
          mutate(label = case_when(
            x == "3" & y == "3" ~ "Both high",
            x == "1" & y == "1" ~ "Both low",
            x == "3" & y == "1" ~ "Canale\nhigh only",
            x == "1" & y == "3" ~ "Housing\nhigh only",
            TRUE ~ NA_character_)) |> 
          mutate(label_colour = if_else(label == "Both high", "white", "black")) |> 
          ggplot(aes(x, y, fill = fill)) +
          geom_tile() +
          geom_text(aes(x, y, label = label, colour = label_colour), 
                    inherit.aes = FALSE, size = 3) +
          scale_fill_manual(values = setNames(colour_bivar_borough$fill[1:9],
                                              colour_bivar_borough$fill[1:9])) +
          scale_colour_manual(values = c("black" = "black", "white" = "white")) +
          labs(x = "Canale (low to high)", y = "Housing (low to high)") +
          theme_void() +
          theme(legend.position = "none")
        
      }
    }, height = {\() if (var_right() == " ") 80 else 120}())
  })
}
