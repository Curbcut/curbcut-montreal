#### RENDER LEGEND #############################################################

render_legend <- function(r, data, var_left, var_right, df, geo, data_type, 
                          build_str_as_DA = TRUE, breaks = NULL) {
  
  ## Clean up data_type and building/street ------------------------------------

  data_type <- sub("building_", "", data_type)
  if (build_str_as_DA && is_scale_in_df(c("building", "street"), df)) 
    df <- paste0(gsub("building", "DA", df))
  df <- gsub(".*_", "", df)
  
  
  ## Get date ------------------------------------------------------------------
  
  date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
  date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
  
  
  ## Get axis titles and breaks ------------------------------------------------
  
  labs_xy <- get_legend_labels(r = r, var_left, var_right, data_type, breaks)
  break_labs <- get_legend_breaks(r = r, data, var_left, var_right, df, geo, 
                                  data_type, breaks)
  
  
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
    } else legend_left_5[1:6,]
    
    leg$x <- as.double(leg$x)
    leg[1,]$x <- 0.5
    leg[seq(2 + 1,nrow(leg) + 1),] <- leg[seq(2,nrow(leg)),]
    leg[2,] <- list(x =  0.75, y = 1, fill = "#FFFFFFFF")

    leg |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(breaks = c(-0.375, 0:5), 
                         labels = as.character(c("NA", break_labs))) +
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
    l$label <- c(cc_t(r = r, "Both low"), " ", 
                 paste0(labs_xy$y_short, "\n", cc_t(r = r, "high only")), " ",
                 " ", " ", 
                 paste0(labs_xy$x_short, "\n", cc_t(r = r, "high only")), " ", 
                 cc_t(r = r, "Both high"))
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
    
    leg <- legend_delta_5
    leg$x <- leg$x
    
    leg$x <- as.double(leg$x)
    leg[seq(1 + 1,nrow(leg) + 1),] <- leg[seq(1,nrow(leg)),]
    leg[1,]$x <- 0.6
    leg[1,]$fill <- "#B3B3BB"
    
    leg[seq(2 + 1,nrow(leg) + 1),] <- leg[seq(2,nrow(leg)),]
    leg[2,] <- list(x =  0.85, y = 1, fill = "#FFFFFFFF")
    
    leg |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(breaks = c(-0.28, 1:4), 
                         labels = as.character( c("NA", "-10%", "-2%", "+2%", "+10%"))) +
      scale_y_continuous(labels = NULL) +
      scale_fill_manual(values = setNames(leg$fill, leg$fill)) + 
      labs_xy + theme_default + theme(axis.text.y = element_blank())
    
    # Bivariate, multi-date
  } else if (data_type %in% c("delta_bivar", "bivar_xdelta_yq3")) {
    
    l <- legend_bivar
    l$label <- c(cc_t(r = r, "Both low"), " ", 
                 paste0(labs_xy$y_short, "\n", cc_t(r = r, "high only")), " ",
                 " ", " ", 
                 paste0(labs_xy$x_short, "\n", cc_t(r = r, "high only")), " ", 
                 cc_t(r = r, "Both high"))
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
