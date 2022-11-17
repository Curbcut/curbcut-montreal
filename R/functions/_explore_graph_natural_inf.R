#### NATURAL INFRASTRUCTURE EXPLORE GRAPH ######################################

explore_graph_natural_inf <- function(data, r = r, var_type, var_left, var_right, 
                                      df, select_id, geo, build_str_as_DA = TRUE) {
                                      
  if (var_left == "c_priority") {
    
    labels <- sapply(c("Flood", "Biodiversity", 
                       "Heat island"), cc_t, r = r,
                     USE.NAMES = FALSE)
    
    data.frame(labels = labels, 
               cons_levels = c(data$c_flood,
                               data$c_biodiversity,
                               data$c_heat_island),
               total_cons = c(1, 1, 1)) |> 
      ggplot() +
      geom_bar(aes(labels, total_cons), stat = "identity", 
               fill = legend_qual$fill[c(3, 2, 4)], alpha = 0.2) +
      geom_bar(aes(labels, cons_levels), stat = "identity", 
               fill = legend_qual$fill[c(3, 2, 4)]) +
      scale_y_continuous(name = NULL, 
                         labels = scales::percent) +
      scale_x_discrete(name = cc_t(r = r, "Amount protected")) +
      theme_minimal() +
      theme(text = element_text(family = "SourceSansPro", size = 12),
            legend.position = "none", 
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.y = element_blank())
    
  } else {
    
    var_titles <- variables[c("var_code", "var_short")]

    dat <- merge(data, variables[c("var_code", "var_short")], by.x = "name", 
                 by.y = "var_code")
    
    dat$var_short <- sapply(dat$var_short, cc_t, r = r, 
                            USE.NAMES = FALSE)
    
    var_names <- dat$var_short[c(4, 9, 3, 6, 2, 5, 7, 8, 1)]
    dat$var_short <- factor(dat$var_short, levels = var_names)
    
    pal <- rev(c(colour_left_5$fill[6:3], colour_iso$fill[c(4, 2)],
                 colour_table$value[colour_table$palette == "qual"][c(3:4, 6)]))
    
    dat |> 
      ggplot(aes(value_pct, var_short, fill = var_short)) +
      geom_col() +
      geom_hline(yintercept = dat$var_short[dat$name == var_left], 
                 colour = "black", lwd = 1) +
      scale_x_continuous(name = cc_t(r = r, "Share of Montreal area"), 
                         labels = scales::label_percent(1)) +
      scale_y_discrete(name = NULL) +
      scale_fill_manual(values = pal) +
      theme_minimal() +
      theme(legend.position = "none")
    
  }
  
}
