#### RENDER LEGEND #############################################################

render_legend <- function(r, data, var_left, var_right, df, data_type, 
                          build_str_as_DA = TRUE, breaks = NULL) {
  
  ## Clean up data_type and building/street ------------------------------------

  data_type <- sub("building_", "", data_type)
  if (build_str_as_DA && is_scale_in_df(c("building", "street"), df)) 
    df <- paste0(gsub("building", "DA", df))
  
  
  ## Get date ------------------------------------------------------------------
  
  date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
  date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
  
  
  ## Get axis titles and breaks ------------------------------------------------
  
  labs_xy <- get_legend_labels(r = r, var_left, var_right, data_type, breaks)
  break_labs <- get_legend_breaks(r = r, data, var_left, var_right, df, data_type,
                                  breaks)
  
  
  ## Prepare default theme -----------------------------------------------------
  
  theme_default <- list(
    theme_minimal(),
    theme(text = element_text(family = "SourceSansPro", size = 11),
          legend.position = "none", 
          panel.grid = element_blank()))
  
  
  ## Render legend -------------------------------------------------------------
  
  # q5 quantitative
  if (data_type == "q5" && !attr(break_labs, "qual")) {
    
    leg <- if (!is.null(breaks) && !is.null(attr(breaks, "palette"))) {
      attr(breaks, "palette")
    } else legend_left_5[2:6,]
    
    leg |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(breaks = 0:5, labels = as.character(break_labs)) +
      scale_y_continuous(labels = NULL) +
      scale_fill_manual(values = setNames(
        leg$fill, leg$fill)) +
      labs_xy + theme_default
    
    # q5 ordinal
  } else if (data_type == "q5" && attr(break_labs, "qual")) {
    
    ranks <- variables$breaks_q5[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
    if (length(ranks) > 0) ranks <- ranks$rank[ranks$scale == df]
    
    legend_left_5[legend_left_5$x %in% ranks,] |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(breaks = ranks - 0.5, labels = break_labs) +
      scale_y_continuous(labels = NULL) +
      scale_fill_manual(values = setNames(
        legend_left_5$fill, legend_left_5$fill)) +
      labs_xy + theme_default
  
    # Qualitative  
  } else if (data_type == "qual") {
    
    ranks <- variables$breaks_q5[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
    if (length(ranks) > 0) ranks <- ranks$rank[ranks$scale == df]
    
    legend_qual[legend_qual$x %in% ranks,] |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(breaks = ranks - 0.5, labels = break_labs) +
      scale_y_continuous(labels = NULL) +
      scale_fill_manual(values = setNames(legend_qual$fill, legend_qual$fill)) +
      labs_xy + theme_default
  
    # Bivariate, single date
  } else if (data_type == "bivar") {
    
    l <- legend_bivar
    l$label <- c(sus_translate(r = r, "Both low"), " ", 
                 paste0(labs_xy$y_short, "\n", sus_translate(r = r, "high only")), " ",
                 " ", " ", 
                 paste0(labs_xy$x_short, "\n", sus_translate(r = r, "high only")), " ", 
                 sus_translate(r = r, "Both high"))
    l$label_colour <- c(rep("black", 8), "white")
    l$x <- as.numeric(l$x) - 0.5
    l$y <- as.numeric(l$y) - 0.5
    
    ggplot(l, aes(y, x, fill = fill)) +
      geom_raster() +
      geom_text(aes(y, x, label = label, colour = label_colour), 
                inherit.aes = FALSE, size = 3) +
      scale_x_continuous(breaks = 0:3, labels = break_labs$x) +
      scale_y_continuous(breaks = 0:3, labels = break_labs$y) +
      scale_fill_manual(values = setNames(
        legend_bivar$fill, legend_bivar$fill)) +
      scale_colour_manual(values = c("black" = "black", "white" = "white")) +
      labs_xy[[1]] + theme_default
    
    # Delta, one date
  } else if (data_type == "delta") {
    
    legend_delta_5 |> 
      ggplot(aes(x, y, fill = fill)) +
      geom_tile() +
      scale_x_continuous(breaks = c(1.5, 2.5, 3.5, 4.5),
                         labels = c("-10%", "-2%", "+2%", "+10%")) +
      scale_fill_manual(values = setNames(
        legend_delta_5$fill, legend_delta_5$fill)) +
      labs_xy + theme_default + theme(axis.text.y = element_blank())
    
    
    # Bivariate, multi-date
  } else if (data_type == "delta_bivar") {
    
    l <- legend_bivar
    l$label <- c(sus_translate(r = r, "Both low"), " ", 
                 paste0(labs_xy$y_short, "\n", sus_translate(r = r, "high only")), " ",
                 " ", " ", 
                 paste0(labs_xy$x_short, "\n", sus_translate(r = r, "high only")), " ", 
                 sus_translate(r = r, "Both high"))
    l$label_colour <- c(rep("black", 8), "white")
    l$x <- as.numeric(l$x) - 0.5
    l$y <- as.numeric(l$y) - 0.5
    
    ggplot(l, aes(y, x, fill = fill)) +
      geom_raster() +
      geom_text(aes(y, x, label = label, colour = label_colour), 
                inherit.aes = FALSE, size = 3#15*0.36
      ) +
      scale_x_continuous(breaks = 0:3, labels = break_labs$x) +
      scale_y_continuous(breaks = 0:3, labels = break_labs$y) +
      scale_fill_manual(values = setNames(
        legend_bivar$fill, legend_bivar$fill)) +
      scale_colour_manual(values = c("black" = "black", "white" = "white")) +
      labs_xy[[1]] + theme_default
    
  } else if (data_type == "q100") {

    leg <- data.frame(x = 1:10, y = 1, fill = scales::viridis_pal()(10))
    
    leg |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(breaks = 0:10, 
                         labels = break_labs) +
      scale_y_continuous(labels = NULL) +
      scale_fill_manual(values = setNames(leg$fill, leg$fill)) +
      labs_xy + theme_default
  } else NULL
  
}
