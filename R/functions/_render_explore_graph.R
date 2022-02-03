#### RENDER EXPLORE GRAPH ######################################################

render_explore_graph <- function(plot_type, data, var_left, var_right, df,
                                 select_id, x_scale, y_scale, labs_xy, 
                                 theme_default) {
  
  # Set convenience variables
  var_left_num <- length(unique(data$var_left))
  bin_number <- min(25, var_left_num)
  
  # Histogram, no selection
  if (plot_type == "hist_all") {
    out <- 
      data |> 
      filter(!is.na(var_left)) |> 
      ggplot(aes(var_left)) +
      geom_histogram(aes(fill = fill), bins = bin_number) +
      scale_fill_manual(values = rev(col_left_5), na.translate = FALSE) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Histogram, NA selection
  if (plot_type == "hist_na") {
    out <- 
      data |> 
      filter(!is.na(var_left)) |> 
      ggplot(aes(var_left)) +
      geom_histogram(bins = bin_number, fill = col_left_5[1]) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Histogram, active selection
  if (plot_type == "hist_select") {
    out <- 
      data |> 
      filter(!is.na(var_left)) |> 
      ggplot(aes(var_left)) +
      geom_histogram(aes(fill = var_left == var_left[ID == select_id]),
                     bins = bin_number) +
      scale_fill_manual(values = col_left_5[c(1, 5)], 
                        na.translate = FALSE) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Bar, no selection
  if (plot_type == "bar_all") {
    
    # Figure out how many values to graph
    ranks <-
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(breaks_q5) |> 
      pluck(1) |> 
      filter(scale == df) |> 
      pull(rank)
    
    # Get corresponding colours
    cols <- set_names(c(col_NA, col_left_5)[ranks + 1])

    out <-
      data |> 
      filter(!is.na(var_left)) |> 
      ggplot(aes(as.factor(var_left))) +
      geom_bar(aes(fill = fill), width = 1) +
      scale_fill_manual(values = cols, na.translate = FALSE) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Bar, NA selection
  if (plot_type == "bar_na") {
    
    # Figure out how many values to graph
    ranks <-
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(breaks_q5) |> 
      pluck(1) |> 
      filter(scale == df) |> 
      pull(rank)
    
    # Get corresponding colours
    cols <- set_names(c(col_NA, col_left_5)[ranks + 1])
    
    out <- 
      data |> 
      filter(!is.na(var_left)) |> 
      ggplot(aes(as.factor(var_left))) +
      geom_bar(fill = col_left_5[1], width = 1) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Bar, active selection
  if (plot_type == "bar_select") {
    
    # Figure out how many values to graph
    ranks <-
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(breaks_q5) |> 
      pluck(1) |> 
      filter(scale == df) |> 
      pull(rank)
    
    # Get corresponding colours
    cols <- set_names(c(col_NA, col_left_5)[ranks + 1])
    
    out <- 
      data |> 
      filter(!is.na(var_left)) |> 
      ggplot(aes(as.factor(var_left))) +
      geom_bar(aes(fill = round(var_left) == 
                     round(var_left[ID == select_id])), 
               width = 1) +
      scale_fill_manual(values = col_left_5[c(1, 5)], 
                        na.translate = FALSE) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Scatterplot, no selection
  if (plot_type == "scatter_all") {
    
    opac <- abs(cor(data$var_left, data$var_right, use = "complete.obs"))
    
    out <- 
      data |> 
      filter(var_left %in% remove_outliers(var_left), 
             var_right %in% remove_outliers(var_right)) |> 
      ggplot(aes(var_right, var_left)) +
      geom_point(aes(colour = group)) +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, alpha = opac) +
      scale_colour_manual(values = tibble::deframe(colour_bivar)) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Scatterplot, NA selection
  if (plot_type == "scatter_na") {
    
    opac <- abs(cor(data$var_left, data$var_right, use = "complete.obs"))
    
    out <- 
      data |> 
      filter(var_left %in% remove_outliers(var_left), 
             var_right %in% remove_outliers(var_right)) |> 
      ggplot(aes(var_right, var_left)) +
      geom_point(colour = col_left_3[1]) +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, alpha = opac) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Scatterplot, active selection
  if (plot_type == "scatter_select") {
    
    opac <- abs(cor(data$var_left, data$var_right, use = "complete.obs"))
    
    out <- 
      data |> 
      filter(var_left %in% remove_outliers(var_left), 
             var_right %in% remove_outliers(var_right)) |> 
      ggplot(aes(var_right, var_left)) +
      geom_point(colour = col_left_3[1]) +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, alpha = opac) +
      geom_point(data = filter(data, ID == select_id),
                 colour = col_bivar[9], size = 3) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Boxplot, no selection
  if (plot_type == "box_all") {
    
    colours <- c(col_left_3[1:2], rep(col_left_3[3], var_left_num - 2))
    names(colours) <- as.factor(unique(sort(data$var_left)))
    
    out <- 
      data |> 
      filter(!is.na(var_left), !is.na(var_right)) |> 
      ggplot(aes(as.factor(var_left), var_right)) +
      geom_boxplot(aes(fill = as.factor(var_left))) +
      scale_fill_manual(values = colours) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Boxplot, NA selection
  if (plot_type == "box_na") {
    
    colours <- c(col_left_3[1:2], rep(col_left_3[3], var_left_num - 2))
    names(colours) <- as.factor(unique(sort(data$var_left)))
    
    out <- 
      data |> 
      filter(!is.na(var_left), !is.na(var_right)) |> 
      ggplot(aes(as.factor(var_left), var_right)) +
      geom_boxplot(fill = col_left_3[1], colour = "grey50") +
      scale_fill_manual(values = colours) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Boxplot, active selection
  if (plot_type == "box_select") {
    
    colours <- c(col_left_3[1:2], rep(col_left_3[3], var_left_num - 2))
    names(colours) <- as.factor(unique(sort(data$var_left)))
    
    out <- 
      data |> 
      filter(!is.na(var_left), !is.na(var_right)) |> 
      ggplot(aes(as.factor(var_left), var_right)) +
      geom_boxplot(fill = col_left_3[1], colour = "grey50") +
      geom_point(data = filter(data, ID == select_id),
                 colour = col_bivar[9], size = 4) +
      scale_fill_manual(values = colours) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Multi-date univariate scatterplot, no selection
  if (plot_type %in% c("delta_all", "NAdelta_all")) {
    
    colours <- colour_delta$fill[1:5]
    names(colours) <- colour_delta$group[1:5]
    
    out <- if (unique(c("var_left_1", "var_left_2") %in% names(data))) {
      data |> 
        filter(var_left_1 %in% remove_outliers(var_left_1), 
               var_left_2 %in% remove_outliers(var_left_2)) |> 
        ggplot(aes(var_left_1, var_left_2)) +
        geom_smooth(se = FALSE, method = "lm", formula = y ~ x, 
                    colour = "black", size = 0.5) +
        geom_point(aes(colour = group)) +
        scale_colour_manual(values = colours) +
        x_scale + y_scale + labs_xy + theme_default
    } else NULL
  }
  
  # Multi-date univariate scatterplot, NA selection
  if (plot_type %in% c("delta_na", "NAdelta_na")) {
    
    out <- if (unique(c("var_left_1", "var_left_2") %in% names(data))) {
      data |> 
        filter(var_left_1 %in% remove_outliers(var_left_1), 
               var_left_2 %in% remove_outliers(var_left_2)) |> 
        ggplot(aes(var_left_1, var_left_2)) +
        geom_smooth(se = FALSE, method = "lm", formula = y ~ x, 
                    colour = "black", size = 0.5) +
        geom_point(colour = col_left_3[1]) +
        x_scale + y_scale + labs_xy + theme_default
    } else NULL
  }
  
  # Multi-date univariate scatterplot, active selection
  if (plot_type %in% c("delta_select", "NAdelta_select")) {
    
    out <- if (unique(c("var_left_1", "var_left_2") %in% names(data))) {
      data |> 
        filter(var_left_1 %in% remove_outliers(var_left_1), 
               var_left_2 %in% remove_outliers(var_left_2)) |> 
        ggplot(aes(var_left_1, var_left_2)) +
        geom_point(colour = col_left_3[1]) +
        geom_smooth(se = FALSE, method = "lm", formula = y ~ x, 
                    colour = "black", size = 0.5) +
        geom_point(data = filter(data, ID == select_id),
                   colour = col_bivar[9], size = 3) +
        x_scale + y_scale + labs_xy + theme_default
    } else NULL
  }
  
  # Multi-date bivariate scatterplot, no selection
  if (plot_type %in% c("deltabi_all", "NAdeltabi_all")) {
    
    opac <- abs(cor(data$var_left, data$var_right, use = "complete.obs"))
    
    out <- 
      data |> 
      filter(var_left %in% remove_outliers(var_left), 
             var_right %in% remove_outliers(var_right)) |> 
      ggplot(aes(var_right, var_left)) +
      geom_point(aes(colour = group)) +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, alpha = opac) +
      scale_colour_manual(values = tibble::deframe(colour_bivar)) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Multi-date bivariate scatterplot, NA selection
  if (plot_type %in% c("deltabi_na", "NAdeltabi_na")) {
    
    opac <- abs(cor(data$var_left, data$var_right, use = "complete.obs"))
    
    out <- 
      data |> 
      filter(var_left %in% remove_outliers(var_left), 
             var_right %in% remove_outliers(var_right)) |> 
      ggplot(aes(var_right, var_left)) +
      geom_point(colour = col_left_3[1]) +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, alpha = opac) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Multi-date bivariate scatterplot, active selection
  if (plot_type %in% c("deltabi_select", "NAdeltabi_select")) {
    
    opac <- abs(cor(data$var_left, data$var_right, use = "complete.obs"))
    
    out <- 
      data |> 
      filter(var_left %in% remove_outliers(var_left), 
             var_right %in% remove_outliers(var_right)) |> 
      ggplot(aes(var_right, var_left)) +
      geom_point(colour = col_left_3[1]) +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, alpha = opac) +
      geom_point(data = filter(data, ID == select_id),
                 colour = col_bivar[9], size = 3) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Date line graph
  if (plot_type == "date_all") {
    out <- ggplot(data, aes(var_right, var_left)) +
      geom_line(colour = col_bivar[5]) +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, colour = col_bivar[9]) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  return(out)
  
}