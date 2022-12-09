#### RENDER EXPLORE GRAPH ######################################################

render_explore_graph <- function(plot_type, data, var_left, var_right, df, geo,
                                 select_id, x_scale, y_scale, labs_xy, 
                                 theme_default) {
  
  # Set convenience variables
  var_left_num <- length(unique(data$var_left))
  bin_number <- max(15, min(25, var_left_num))
  
  # Histogram
  if (plot_type %in% c("hist_all", "hist_na", "hist_select")) {
    
    vals <- variables$breaks_q5[[
      which(variables$var_code == str_remove(var_left, "_\\d{4}$"))]]
    vals <- vals$var[vals$scale == gsub(".*_", "", df) &
                       vals$geo == geo]
    vals[1] <- -Inf
    vals[length(vals)] <- Inf

    out <- 
      data[!is.na(data$var_left),] |> 
      ggplot(aes(var_left)) +
      geom_histogram(aes(fill = after_stat(x)), bins = bin_number) +
      {if (plot_type == "hist_select") geom_vline(
        xintercept = data$var_left[data$ID == select_id], colour = "black", 
        lwd = 1.5)} +
      binned_scale(aesthetics = "fill",
                   scale_name = "stepsn",
                   palette = \(x) colour_left_5$fill[2:6],
                   breaks = vals) +
      x_scale + y_scale + labs_xy + theme_default
    
  }
  
  # Bar, no selection
  if (plot_type %in% c("bar_all", "bar_na", "bar_select")) {
    
    # Figure out how many values to graph
    ranks <- variables$breaks_q5[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
    ranks <- ranks$rank[ranks$scale == gsub(".*_", "", df) &
                          ranks$geo == geo]
    
    # Get corresponding colours
    cols <- colour_left_5$fill[ranks + 1]
    
    out <-
      data[!is.na(data$var_left),] |> 
      ggplot(aes(as.factor(var_left))) +
      geom_bar(aes(fill = as.factor(var_left)), width = 1) +
      {if (plot_type == "bar_select") geom_vline(
        xintercept = data$var_left[data$ID == select_id], colour = "black", 
        lwd = 1.5)} +
      scale_fill_manual(values = cols, na.translate = FALSE) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Scatterplot
  if (plot_type %in% c("scatter_all", "scatter_na", "scatter_select")) {
    
    opac_line <- abs(cor(data$var_left, data$var_right, use = "complete.obs"))
    point_size <- if (nrow(data) > 1000) {
      0.5
    } else if (nrow(data) > 500) {
      1
    } else 2
    
    out <-
      data |> 
      remove_outliers_df("var_left", "var_right") |> 
      ggplot(aes(var_right, var_left)) +
      geom_point(aes(colour = group), size = point_size) +
      {if (plot_type == "scatter_select") geom_point(
        data = data[data$ID == select_id,], shape = 21, colour = "white", 
        fill = "black", size = 4)} +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, alpha = opac_line) +
      scale_colour_manual(values = setNames(
        colour_bivar$fill, colour_bivar$group)) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Boxplot, no selection
  if (plot_type %in% c("box_all", "box_na", "box_select")) {
    
    colours <- c(colour_bivar$fill[1:2], rep(colour_bivar$fill[3], 
                                             var_left_num - 2))
    names(colours) <- as.factor(unique(sort(data$var_left)))
    
    out <- 
      data[!is.na(data$var_left) & !is.na(data$var_right),] |> 
      ggplot(aes(as.factor(var_left), var_right)) +
      geom_boxplot(aes(fill = as.factor(var_left))) +
      {if (plot_type == "box_select") geom_point(
        data = data[data$ID == select_id,], shape = 21, colour = "white", 
        fill = "black", size = 4)} +
      scale_fill_manual(values = colours) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Multi-date univariate scatterplot
  if (plot_type %in% c("delta_all", "NAdelta_all", "delta_na", "NAdelta_na",
                       "delta_select", "NAdelta_select")) {
    
    colours <- colour_delta$fill[1:5]
    names(colours) <- colour_delta$group[1:5]
    
    out <- if (unique(c("var_left_1", "var_left_2") %in% names(data))) {
      data |> 
        remove_outliers_df("var_left_1", "var_left_2") |> 
        ggplot(aes(var_left_1, var_left_2)) +
        geom_smooth(se = FALSE, method = "lm", formula = y ~ x, 
                    colour = "black", size = 0.5) +
        geom_point(aes(colour = group)) +
        {if (plot_type == "delta_select") geom_point(
          data = data[data$ID == select_id,], shape = 21, colour = "white", 
          fill = "black", size = 4)} +
        scale_colour_manual(values = colours) +
        x_scale + y_scale + labs_xy + theme_default
    } else NULL
  }
  
  # Multi-date bivariate scatterplot
  if (plot_type %in% c("deltabivar_all", "NAdeltabivar_all", 
                       "deltabivar_na", "NAdeltabivar_na",
                       "deltabivar_select", "NAdeltabivar_select")) {
    
    opac_line <- if (sum(!is.na(data$var_left)) > 0) {
      abs(cor(data$var_left, data$var_right, use = "complete.obs"))
    } else 1
    
    out <- 
      data |> 
      remove_outliers_df("var_left", "var_right") |> 
      ggplot(aes(var_right, var_left)) +
      geom_point(aes(colour = group)) +
      {if (plot_type == "deltabivar_select") geom_point(
        data = data[data$ID == select_id,], shape = 21, colour = "white", 
        fill = "black", size = 4)} +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, alpha = opac_line) +
      scale_colour_manual(values = setNames(
        colour_bivar$fill, colour_bivar$group)) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  # Date line graph
  if (plot_type == "date_all") {
    out <- ggplot(data, aes(var_right, var_left)) +
      geom_line(colour = colour_bivar$fill[5]) +
      stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                  formula = y ~ x, colour = colour_bivar$fill[9]) +
      x_scale + y_scale + labs_xy + theme_default
  }
  
  return(out)
  
}