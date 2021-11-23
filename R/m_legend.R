#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  tagList(
      h5("Legend", style = "font-size: 11px;"),
      uiOutput(NS(id, "legend_render"))
  )
}

legend_server <- function(id, var_left, var_right, zoom_val) {
  
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(zoom_val))
  
  moduleServer(id, function(input, output, session) {
    
    plot_height <- function() {
      if (var_right() == " ") 80 else 120
    }

    output$legend_render <- renderUI({
      
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
          
          var_left <- str_remove(var_left(), "_\\d{4}$")
          var_right <- str_remove(var_right(), "_\\d{4}$")
          var_left_title <- 
            var_exp |> 
            filter(var_code == var_left) |> 
            pull(var_name)
          
          var_right_title <- 
            var_exp |> 
            filter(var_code == var_right) |> 
            pull(var_name)
          
          legend_bivar |>
            mutate(label = case_when(
              x == "3" & y == "3" ~ "Both high",
              x == "1" & y == "1" ~ "Both low",
              x == "3" & y == "1" ~ paste0(var_left_title, "\nhigh only"),
              x == "1" & y == "3" ~ paste0(var_right_title, "\nhigh only"),
              TRUE ~ NA_character_)) |> 
            mutate(label_colour = if_else(label == "Both high", "white", "black")) |> 
            ggplot(aes(y, x, fill = fill)) +
            geom_tile() +
            geom_text(aes(y, x, label = label, colour = label_colour), 
                      inherit.aes = FALSE, size = 3) +
            scale_fill_manual(values = setNames(
              paste0(legend_bivar$fill, 
                     filter(colour_alpha, zoom == zoom_val())$alpha),
              legend_bivar$fill)) +
            scale_colour_manual(values = c("black" = "black", "white" = "white")) +
            labs(x = paste0(var_right_title, " (low to high)"), 
                 y = paste0(var_left_title, "Housing (low to high)")) +
            theme_void() +
            theme(legend.position = "none")
          
        }
      })
      
      plotOutput("canale-legend-legend", height = plot_height(), width = "100%")
      
    })
  
  })
}
