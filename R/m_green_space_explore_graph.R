#### GREEN SPACE EXPLORE MODULE ###############################################

green_space_explore_graph <- function(id, x, select_id, ...) {
  
  moduleServer(id, function(input, output, session) {
    reactive({
      
      if (is.na(select_id())) {
        ggplot(x(), aes(area)) +
          geom_histogram(aes(fill = fill), alpha = 0.5, bins = 25) +
          scale_fill_manual(values = rev(colour_left_5$fill[2:6]), 
                            na.translate = FALSE) + 
          labs(x = "Green space area (log10)", y = NULL) + 
          scale_x_log10() +
          theme_minimal() +
          theme(legend.position = "none", panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.title = element_text(size = 8))
      } else {
        select_id <- select_id()
        
        ggplot(x(), aes(area)) +
          geom_histogram(aes(fill = round(area) == 
                               round(area[ID == select_id])),
                         bins = 25) +
          scale_fill_manual(values = colour_left_5$fill[c(2, 6)], 
                            na.translate = FALSE) +
          labs(x = "Green space area (log10)", y = NULL) + 
          scale_x_log10() +
          theme_minimal() +
          theme(legend.position = "none", panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.title = element_text(size = 8))
      }
      
    })
  })
}
