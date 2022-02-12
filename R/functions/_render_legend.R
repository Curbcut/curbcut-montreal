#### RENDER LEGEND #############################################################

render_legend <- function(data, var_left, var_right, df, zoom = df, data_type, 
                          build_str_as_DA = TRUE) {
  
  ## Clean up data_type and building/street ------------------------------------
  
  data_type <- sub("building_", "", data_type)
  if (build_str_as_DA && df %in% c("building", "street")) df <- "DA"
  
  
  ## Get date ------------------------------------------------------------------
  
  date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
  date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
  
  
  ## Get axis titles and breaks ------------------------------------------------

  labs_xy <- get_legend_labels(var_left, var_right, data_type)
  break_labels <- get_legend_breaks(data, var_left, var_right, df, data_type)

  
  ## Get opacity ---------------------------------------------------------------
  
  opac <- colour_alpha[names(colour_alpha) == zoom]
  if (length(opac) == 0) opac <- "FF"
  
  
  ## Prepare default theme -----------------------------------------------------
  
  theme_default <- list(
    theme_minimal(),
    theme(legend.position = "none", 
          panel.grid = element_blank(),
          axis.title = element_text(size = 8)))
  
  
  ## Render legend -------------------------------------------------------------
  
  # q5 quantitative
  if (data_type == "q5" && !attr(break_labels, "qual")) {
    
    legend_left_5 |> 
      mutate(fill = paste0(fill, opac)) |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(breaks = 0:5, labels = as.character(break_labels)) +
      scale_y_continuous(labels = NULL) +
      scale_fill_manual(values = set_names(paste0(legend_left_5$fill, opac))) +
      labs_xy + theme_default
  
  # q5 qualitative
  } else if (data_type == "q5" && attr(break_labels, "qual")) {
    
    ranks <-
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(breaks_q5) |> 
      pluck(1) |> 
      filter(scale == df) |> 
      pull(rank)
    
    legend_left_5 |> 
      add_row(x = 0, y = 1, fill = col_NA, .before = 1) |> 
      mutate(fill = paste0(fill, opac)) |> 
      filter(x %in% ranks) |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(breaks = ranks - 0.5, labels = break_labels) +
      scale_y_continuous(labels = NULL) +
      scale_fill_manual(values = set_names(paste0(legend_left_5$fill, opac))) +
      labs_xy + theme_default
    
  # Bivariate, single date
  } else if (data_type == "bivar") {
    
    legend_bivar |>
      mutate(label = case_when(
        x == "3" & y == "3" ~ "Both high",
        x == "1" & y == "1" ~ "Both low",
        x == "3" & y == "1" ~ paste0(labs_xy$y_short, "\nhigh only"),
        x == "1" & y == "3" ~ paste0(labs_xy$x_short, "\nhigh only"),
        TRUE ~ " ")) |> 
      mutate(label_colour = if_else(
        label == "Both high", "white", "black")) |> 
      mutate(x = as.numeric(x) - 0.5,
             y = as.numeric(y) - 0.5) |> 
      mutate(fill = paste0(fill, opac)) |> 
      ggplot(aes(y, x, fill = fill)) +
      geom_raster() +
      geom_text(aes(y, x, label = label, colour = label_colour), 
                inherit.aes = FALSE, size = 3#15*0.36
                ) +
      scale_x_continuous(breaks = 0:3, labels = break_labels$x) +
      scale_y_continuous(breaks = 0:3, labels = break_labels$y) +
      scale_fill_manual(values = set_names(paste0(legend_bivar$fill, opac))) +
      scale_colour_manual(values = c("black" = "black", "white" = "white")) +
      labs_xy[[1]] + theme_default
    
  # Delta, one date
  } else if (data_type == "delta") {
    
    legend_delta_5 |> 
      mutate(fill = paste0(fill, opac)) |> 
      ggplot(aes(x, y, fill = fill)) +
      geom_tile() +
      scale_x_continuous(breaks = c(1.5, 2.5, 3.5, 4.5),
                         labels = c("-10%", "-2%", "+2%", "+10%")) +
      scale_fill_manual(values = setNames(paste0(legend_delta_5$fill, opac),
                                          paste0(legend_delta_5$fill, opac))) +
      labs_xy + theme_default + theme(axis.text.y = element_blank())
    
    
  # Bivariate, multi-date
  } else if (data_type == "delta_bivar") {
    
    legend_bivar |>
      mutate(label = case_when(
        x == "3" & y == "3" ~ "Both high",
        x == "1" & y == "1" ~ "Both low",
        x == "3" & y == "1" ~ paste0(labs_xy$y_short, "\nhigh only"),
        x == "1" & y == "3" ~ paste0(labs_xy$x_short, "\nhigh only"),
        TRUE ~ " ")) |> 
      mutate(label_colour = if_else(
        label == "Both high", "white", "black")) |> 
      mutate(x = as.numeric(x) - 0.5,
             y = as.numeric(y) - 0.5) |> 
      mutate(fill = paste0(fill, opac)) |> 
      ggplot(aes(y, x, fill = fill)) +
      geom_raster() +
      geom_text(aes(y, x, label = label, colour = label_colour), 
                inherit.aes = FALSE, size = 3#15*0.36
      ) +
      scale_x_continuous(breaks = 0:3, labels = break_labels$x) +
      scale_y_continuous(breaks = 0:3, labels = break_labels$y) +
      scale_fill_manual(values = set_names(paste0(legend_bivar$fill, opac))) +
      scale_colour_manual(values = c("black" = "black", "white" = "white")) +
      labs_xy[[1]] + theme_default
    
  } else NULL
  
}
