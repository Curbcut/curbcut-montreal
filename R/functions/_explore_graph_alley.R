#### GREEN AlLEY EXPLORE GRAPH #################################################


explore_graph_alley <- function(data, var_type, var_left, var_right, df, 
                                select_id, build_str_as_DA = TRUE) {
  
  # Show histogram in borough summary mode
  if (df == "borough_empty") {
    alley[!is.na(alley$created),] |> 
      ggplot(aes(created)) +
      geom_histogram(fill = colour_left_5$fill[3]) +
      scale_y_continuous(name = NULL) +
      scale_x_continuous(name = sus_translate("Green alley start date")) +
      theme_minimal() +
      theme(text = element_text(family = "SourceSansPro", size = 13),
            legend.position = "none", 
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.y = element_blank())
    
  # Show nothing in alley selection mode?
  } else if (df == "alley") {
    NULL
  }
  
}