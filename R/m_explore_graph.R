#### EXPLORE GRAPH MODULE ######################################################

explore_graph_UI <- function(id) {
  plotOutput(NS(id, "explore_graph"), height = 150)
}

explore_graph_server <- function(id, x, var_type, var_left, var_right, select, 
                                 var_left_title, var_left_label = NULL, 
                                 var_right_label = NULL, plot_type = "auto") {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_type))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select))
  stopifnot(is.reactive(var_left_title))
  
  moduleServer(id, function(input, output, session) {
    
    output$explore_graph <- renderPlot({
      
      dat <- tidyr::drop_na(x())
      
      # Set convenience variables
      left_var_num <- length(unique(dat$left_var_full))
      bin_number <- min(25, left_var_num)
      var_name <- sus_translate(var_exp %>%
                                  filter(var_code == var_right()) %>%
                                  pull(var_name))
      active_select <- nrow(filter(dat, ID == select()))
      na_select <- nrow(filter(dat, ID == select(), !is.na(left_var)))
      
      # Decide on plot type
      if (plot_type == "auto") {
        graph_type <- case_when(
          var_right() == " " & left_var_num > 6 ~ "hist",
          var_right() == " " & left_var_num <= 6 ~ "bar",
          var_right() != " " & left_var_num > 6 ~ "scatter",
          var_right() != " " & left_var_num <= 6 ~ "box")
        
        select_type <- case_when(is.na(select()) ~ "all", na_select == 0 ~ "na",
                                 TRUE ~ "select")
        plot_type <- paste(graph_type, select_type, sep = "_")
        }
      
      # Prepare x scale
      x_scale <- case_when(
        # Discrete scale
        !is.null(var_left_label) & graph_type %in% c("bar", "box") ~
          list(scale_x_discrete(labels = var_left_label)),
        # Continuous scale, labels
        !is.null(var_left_label) & graph_type == "hist" ~ 
          list(scale_x_continuous(
            limits = c(min(0, as.numeric(names(var_left_label))),
                       max(0, as.numeric(names(var_left_label)))),
            breaks = as.numeric(names(var_left_label)),
            labels = var_left_label)),
        # Continuous scale, percent
        stringr::str_detect(var_left(), "prop") ~
          list(scale_x_continuous(labels = scales::percent)),
        # Continuous scale, dollar
        stringr::str_detect(var_left(), "dollar") ~
          list(scale_x_continuous(labels = scales::dollar)),
        # Continuous scale, comma
        TRUE ~ list(scale_x_continuous(labels = scales::comma))
        )

      # Prepare y scale
      y_scale <- case_when(
        # Continuous scale, comma, no decimal
        graph_type %in% c("hist", "bar") ~ 
          list(scale_y_continuous(labels = scales::label_comma(accuracy = 1))),
        # Continuous scale, percent
        stringr::str_detect(var_right(), "prop") ~ 
          list(scale_y_continuous(labels = scales::percent)),
        # Continuous scale, dollar
        stringr::str_detect(var_right(), "dollar") ~ 
          list(scale_y_continuous(labels = scales::dollar)),
        # Continuous scale, comma
        TRUE ~ list(scale_y_continuous(labels = scales::comma))
        )
      
      # Prepare axis labels
      labs_x <- list(labs(x = sus_translate(var_left_title()), y = NULL))
      labs_xy <- list(labs(x = sus_translate(var_left_title()), 
                           y = sus_translate(var_name)))
      
      # Prepare default theme
      theme_default <- list(
        theme_minimal(),
        theme(legend.position = "none", panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()))
      
      # Histogram, no selection
      if (plot_type == "hist_all") {
        out <- ggplot(dat, aes(left_var_full)) +
          geom_histogram(aes(fill = fill), bins = bin_number) +
          scale_fill_manual(values = colour_scale[3:1], na.translate = FALSE) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Histogram, NA selection
      if (plot_type == "hist_na") {
        out <- ggplot(dat, aes(left_var_full)) +
          geom_histogram(bins = bin_number, fill = colour_scale[1]) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Histogram, active selection
      if (plot_type == "hist_select") {
        out <- ggplot(dat, aes(left_var_full)) +
          geom_histogram(aes(fill = round(left_var_full) ==
                               round(left_var_full[ID == select()])),
                         bins = bin_number) +
          scale_fill_manual(values = colour_scale[c(1, 3)], 
                            na.translate = FALSE) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Bar, no selection
      if (plot_type == "bar_all") {
        out <- ggplot(dat, aes(as.factor(left_var_full))) +
          geom_bar(aes(fill = fill), width = 1) +
          scale_fill_manual(values = colour_scale[3:1], na.translate = FALSE) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Bar, NA selection
      if (plot_type == "bar_na") {
        out <- ggplot(dat, aes(as.factor(left_var_full))) +
          geom_bar(fill = colour_scale[1], width = 1) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Bar, active selection
      if (plot_type == "bar_select") {
        out <- ggplot(dat, aes(as.factor(left_var_full))) +
          geom_bar(aes(fill = round(left_var_full) == 
                               round(left_var_full[ID == select()])), 
                         width = 1) +
          scale_fill_manual(values = colour_scale[c(1, 3)], 
                            na.translate = FALSE) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Scatterplot, no selection
      if (plot_type == "scatter_all") {
        out <- ggplot(dat, aes(left_var_full, right_var_full)) +
          geom_point(aes(colour = group)) +
          scale_colour_manual(values = tibble::deframe(colour_bivar)) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Scatterplot, NA selection
      if (plot_type == "scatter_na") {
        out <- ggplot(dat, aes(left_var_full, right_var_full)) +
          geom_point(colour = colour_bivar$fill[9]) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Scatterplot, active selection
      if (plot_type == "scatter_select") {
        out <- ggplot(dat, aes(left_var_full, right_var_full)) +
          geom_point(colour = colour_bivar$fill[9]) +
          geom_point(data = filter(dat, ID == select()),
                     colour = colour_bivar$fill[1], size = 3) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Boxplot, no selection
      if (plot_type == "box_all") {
        
        colours <- c(colour_scale[1:2], rep(colour_scale[3], left_var_num - 2))
        names(colours) <- as.factor(unique(sort(dat$left_var_full)))
        
        out <- ggplot(dat, aes(as.factor(left_var_full), right_var_full)) +
          geom_boxplot(aes(fill = as.factor(left_var_full))) +
          scale_fill_manual(values = colours) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Boxplot, NA selection
      if (plot_type == "box_na") {
        
        colours <- c(colour_scale[1:2], rep(colour_scale[3], left_var_num - 2))
        names(colours) <- as.factor(unique(sort(dat$left_var_full)))
        
        out <- ggplot(dat, aes(as.factor(left_var_full), right_var_full)) +
          geom_boxplot(fill = colour_scale[1], colour = "grey50") +
          scale_fill_manual(values = colours) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Boxplot, active selection
      if (plot_type == "box_select") {
        
        colours <- c(colour_scale[1:2], rep(colour_scale[3], left_var_num - 2))
        names(colours) <- as.factor(unique(sort(dat$left_var_full)))
        
        out <- ggplot(dat, aes(as.factor(left_var_full), right_var_full)) +
          geom_boxplot(fill = colour_scale[1], colour = "grey50") +
          geom_point(data = filter(dat, ID == select()),
                     colour = colour_bivar$fill[1], size = 4) +
          scale_fill_manual(values = colours) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      out
    }, bg = "white")
  })
}
