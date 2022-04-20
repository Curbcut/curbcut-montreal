#### NATURAL INFRASTRUCTURE EXPLORE GRAPH ######################################


explore_graph_natural_inf <- function(data, var_type, var_left, var_right, df, 
                                                 select_id, build_str_as_DA = TRUE) {

  if (!is.null(data)) {
    
    labels <- sapply(c("Flood", "Biodiversity", "Heat island"), sus_translate, 
                     USE.NAMES = FALSE)
    
    data.frame(labels = labels, 
               cons_levels = c(data$flood_prevention,
                               data$biodiversity_conservation,
                               data$heat_island_reduction),
               total_cons = c(1,1,1)) |> 
      ggplot() +
      geom_bar(aes(labels, total_cons), stat = "identity", 
               fill = legend_qual$fill[c(3,2,4)], alpha = 0.2) +
      geom_bar(aes(labels, cons_levels), stat = "identity", 
               fill = legend_qual$fill[c(3,2,4)]) +
      scale_y_continuous(name = NULL, labels = scales::percent) +
      scale_x_discrete(name = sus_translate("Prevention conservation")) +
      theme_minimal() +
      theme(text = element_text(family = "SourceSansPro", size = 12),
            legend.position = "none", 
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.y = element_blank())
    
  } else NULL
  
}
