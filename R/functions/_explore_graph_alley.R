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
    if (is.na(select_id)) {
      
      labels <- 
        variables$breaks_q5[variables$var_code == "alley_qual"] |> 
        as.data.frame() |> 
        pull(var_name_short)
      
      ranks <- variables$breaks_q5[
        variables$var_code == unique(sub("_\\d{4}$", "", var_left))]
      if (length(ranks) > 0) ranks <- ranks[[1]]$rank[ranks[[1]]$scale == df]

      alley[!is.na(alley$created),] |> 
        ggplot(aes(type)) +
        geom_histogram(fill = legend_qual[legend_qual$x %in% ranks,]$fill[1:4], 
                       stat = "count") +
        scale_y_continuous(name = NULL) +
        scale_x_discrete(labels = labels,
                         name = sus_translate("Visited green alleys type")) +
        theme_minimal() +
        theme(text = element_text(family = "SourceSansPro", size = 13),
              legend.position = "none", 
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(), 
              panel.grid.minor.y = element_blank())
    } else NULL
  }
  
}