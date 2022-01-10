render_plot_fun <- function(var_left, var_right, df, data_type) {
  
  ## One variable, one date, q5 --------------------------------------------
  
  if (data_type %in% c("q5", "building_q5")) {
    
    # Get date for convenience
    date_left <- str_extract(var_left(), "(?<=_)\\d{4}$")
    
    # Get axis title
    axis_title <- sus_translate(
      variables |> 
        filter(var_code == unique(sub("_\\d{4}$", "", var_left()))) |> 
        pull(var_short))
    
    # Get break labels
    break_labels <- 
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left()))) |> 
      pull(breaks_q5) |> 
      purrr::pluck(1) |> 
      filter(date == date_left | is.na(date))
    
    if (df() %in% c("building", "street") &&
        nrow(filter(break_labels, scale == df())) == 0) {
      break_labels <- 
        break_labels |> 
        filter(scale == "DA") |> 
        pull(var)
    } else {
      break_labels <- 
        break_labels |> 
        filter(scale == df()) |> 
        pull(var)
    }
    
    # Format break labels
    if (str_detect(var_left(), "_pct|_dollar")) {
      break_labels <- convert_unit(break_labels, var_left())
    }
    
    # Draw legend
    legend_left_5 |> 
      ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                 fill = fill)) +
      geom_rect() + 
      scale_x_continuous(name = axis_title, breaks = 0:5,
                         labels = as.character(break_labels)) +
      scale_y_continuous(name = NULL, labels = NULL) +
      scale_fill_manual(values = set_names(legend_left_5$fill)) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            # axis.text = element_text(size = 15),
            # axis.title = element_text(size = 15)
            )
  
    
  # Bivariate, one date, q3 ----------------------------------------------------
    
  } else if (data_type %in% c("bivar", "building_bivar")) {
    
    # Get dates for convenience
    date_left <- str_extract(var_left(), "(?<=_)\\d{4}$")
    date_right <- str_extract(var_right(), "(?<=_)\\d{4}$")
    
    # Get axis titles
    axis_title_y <- sus_translate(
      variables |> 
        filter(var_code == unique(sub("_\\d{4}$", "", var_left()))) |> 
        pull(var_short))
    
    axis_title_x <- sus_translate(
      variables |> 
        filter(var_code == unique(sub("_\\d{4}$", "", var_right()))) |> 
        pull(var_short))
    
    # Get breaks
    break_labels_y <- 
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left()))) |> 
      pull(breaks_q3) |> 
      pluck(1) |> 
      filter(date == date_left | is.na(date))
    
    if (df() %in% c("building", "street") &&
        nrow(filter(break_labels_y, scale == df())) == 0) {
      break_labels_y <- 
        break_labels_y |> 
        filter(scale == "DA") |> 
        pull(var)
    } else {
      break_labels_y <- 
        break_labels_y |> 
        filter(scale == df()) |> 
        pull(var)
    }
    
    break_labels_x <- 
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_right()))) |> 
      pull(breaks_q3) |> 
      pluck(1) |> 
      filter(date == date_right | is.na(date))
    
    if (df() %in% c("building", "street") &&
        nrow(filter(break_labels_x, scale == df())) == 0) {
      break_labels_x <- 
        break_labels_x |> 
        filter(scale == "DA") |> 
        pull(var)
    } else {
      break_labels_x <- 
        break_labels_x |> 
        filter(scale == df()) |> 
        pull(var)
    }
    
    # Format breaks
    break_labels_y <- convert_unit(break_labels_y, var_left())
    break_labels_x <- convert_unit(break_labels_x, var_right())
    
    legend_bivar |>
      mutate(label = case_when(
        x == "3" & y == "3" ~ "Both high",
        x == "1" & y == "1" ~ "Both low",
        x == "3" & y == "1" ~ paste0(axis_title_y, "\nhigh only"),
        x == "1" & y == "3" ~ paste0(axis_title_x, "\nhigh only"),
        TRUE ~ " ")) |> 
      mutate(label_colour = if_else(
        label == "Both high", "white", "black")) |> 
      mutate(x = as.numeric(x) - 0.5,
             y = as.numeric(y) - 0.5) |> 
      ggplot(aes(y, x, fill = fill)) +
      geom_raster() +
      geom_text(aes(y, x, label = label, colour = label_colour), 
                inherit.aes = FALSE, size = 3#15*0.36
                ) +
      scale_x_continuous(name = axis_title_x, breaks = 0:3, 
                         labels = break_labels_x) +
      scale_y_continuous(name = axis_title_y, breaks = 0:3, 
                         labels = break_labels_y) +
      scale_fill_manual(values = set_names(legend_bivar$fill)) +
      scale_colour_manual(values = c(
        "black" = "black", "white" = "white")) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            # axis.text = element_text(size = 15),
            # axis.title = element_text(size = 15)
            )
    
  } else if (length(var_left()) == 2 && var_right()[1] == " ") {
    
    legend_delta_5 |> 
      ggplot(aes(x, y, fill = fill)) +
      geom_tile() +
      scale_x_continuous(name = "var_name (change DATE_1 - DATE_2)",
                         breaks = c(1.5, 2.5, 3.5, 4.5),
                         labels = c("-10%", "-2%", "+2%", "+10%")) +
      scale_y_continuous(name = NULL) +
      scale_fill_manual(values = setNames(
        paste0(legend_delta_5$fill, 
               filter(colour_alpha, zoom == "borough")$alpha),
        legend_delta_5$fill)) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            panel.grid = element_blank(),
            # axis.text = element_text(size = 15),
            # axis.title = element_text(size = 15)
            )
    
  } else {
    
    var_left <- unique(str_remove(var_left(), "_\\d{4}$"))
    var_right <- unique(str_remove(var_right(), "_\\d{4}$"))
    
    var_left_title <- 
      variables |> 
      filter(var_code == var_left) |> 
      pull(var_short)
    
    var_right_title <- 
      variables |> 
      filter(var_code == var_right) |> 
      pull(var_short)
    
    legend_bivar |>
      mutate(label = case_when(
        x == "3" & y == "3" ~ "Both high",
        x == "1" & y == "1" ~ "Both low",
        x == "3" & y == "1" ~ paste0(var_left_title, "\nhigh only"),
        x == "1" & y == "3" ~ paste0(var_right_title, "\nhigh only"),
        TRUE ~ NA_character_)) |> 
      mutate(label_colour = if_else(
        label == "Both high", "white", "black")) |> 
      ggplot(aes(y, x, fill = fill)) +
      geom_tile() +
      geom_text(aes(y, x, label = label, colour = label_colour), 
                inherit.aes = FALSE, size = 3#15*0.36
                ) +
      scale_fill_manual(values = setNames(
        paste0(legend_bivar$fill, 
               filter(colour_alpha, zoom == df())$alpha),
        legend_bivar$fill)) +
      scale_colour_manual(values = c(
        "black" = "black", "white" = "white")) +
      labs(x = paste0(var_right_title, " (low to high)"), 
           y = paste0(var_left_title, "Housing (low to high)")) +
      theme_void() +
      theme(legend.position = "none",
            # axis.text = element_text(size = 15),
            # axis.title = element_text(size = 15)
            )
    
  }
}
