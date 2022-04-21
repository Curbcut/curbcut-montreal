#### NATURAL INFRASTRUCTURE EXPLORE GRAPH ######################################


explore_graph_natural_inf <- function(data, var_type, var_left, var_right, df, 
                                      select_id, build_str_as_DA = TRUE) {

  print(var_left)
  if (var_left == "c_priority") {
    
    labels <- sapply(c("Flood prevention", "Biodiversity cons.", 
                       "Heat island red."), sus_translate, 
                     USE.NAMES = FALSE)
    
    data.frame(labels = labels, 
               cons_levels = c(data$flood,
                               data$biodiversity,
                               data$heat_island),
               total_cons = c(1, 1, 1)) |> 
      ggplot() +
      geom_bar(aes(labels, total_cons), stat = "identity", 
               fill = legend_qual$fill[c(3, 2, 4)], alpha = 0.2) +
      geom_bar(aes(labels, cons_levels), stat = "identity", 
               fill = legend_qual$fill[c(3, 2, 4)]) +
      scale_y_continuous(name = NULL, 
                         labels = scales::percent) +
      scale_x_discrete(name = sus_translate("Amount protected")) +
      theme_minimal() +
      theme(text = element_text(family = "SourceSansPro", size = 12),
            legend.position = "none", 
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.y = element_blank())
    
  } else {
    data |> 
      ggplot(aes(value_pct, name, fill = name)) +
      geom_col() +
      scale_x_continuous(name = sus_translate("Share of Montreal area"), 
                         labels = scales::percent) +
      scale_y_discrete(name = NULL) +
      scale_fill_manual(
        values = rep(colour_table$value[colour_table$palette == "qual"], 2)) +
      theme_minimal() +
      theme(legend.position = "none")
    
  }
  
}
