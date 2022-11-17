#### GREEN AlLEY EXPLORE GRAPH #################################################


explore_graph_alley <- function(r = r, data, var_type, var_left, var_right, df, 
                                select_id, geo, build_str_as_DA = TRUE) {
  
  # Show histogram in borough summary mode
  if (is_scale_in_df("borough_empty", df)) {
    alley[!is.na(alley$created),] |> 
      ggplot(aes(created)) +
      geom_histogram(fill = colour_left_5$fill[3], bins = 30) +
      scale_y_continuous(name = NULL) +
      scale_x_continuous(name = cc_t(r = r, "Green alley start date")) +
      theme_minimal() +
      theme(text = element_text(family = "SourceSansPro", size = 12),
            legend.position = "none", 
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.y = element_blank())
    
  # Show type-summary histogram or NULL in alley mode
  } else if (is_scale_in_df("alley", df)) {
    if (is.na(select_id)) {
      
      labels <- variables$breaks_q5[[
        which(variables$var_code == "alley_qual")]]$var_name_short
      
      labels <- sapply(labels, cc_t, r = r, USE.NAMES = FALSE)
      
      ranks <- variables$breaks_q5[[
        which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
      if (length(ranks) > 0) ranks <- ranks$rank[ranks$scale == df]

      alley[!is.na(alley$created),] |> 
        ggplot(aes(type)) +
        geom_bar(fill = legend_qual[legend_qual$x %in% ranks,]$fill[1:4]) +
        scale_y_continuous(name = NULL) +
        scale_x_discrete(labels = labels,
                         name = cc_t(r = r, "Visited green alleys type")) +
        theme_minimal() +
        theme(text = element_text(family = "SourceSansPro", size = 12),
              legend.position = "none", 
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(), 
              panel.grid.minor.y = element_blank())
    } else NULL
  }
  
}
