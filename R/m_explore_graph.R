#### EXPLORE GRAPH MODULE ######################################################

explore_graph_UI <- function(id) {
  plotOutput(NS(id, "explore_graph"), height = 150)
}

explore_graph_server <- function(id, x, var_type, var_left, var_right, select, 
                                 zoom, var_left_label = NULL, 
                                 var_right_label = NULL, build_str_as_DA = TRUE,
                                 plot_type = "auto") {
  
  # Check arguments
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_type))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select))
  stopifnot(is.reactive(zoom))

  # Server function
  moduleServer(id, function(input, output, session) {
    
    output$explore_graph <- renderPlot({
      
      # Set convenience variables, and deal with build_str_as_DA ---------------
      
      if (!zoom() %in% c("building", "street")) build_str_as_DA <- FALSE
      
      if (build_str_as_DA) {
        tb <- data_server(id = "explore_graph",
                          var_left = var_left,
                          var_right = var_right,
                          df = reactive("DA"))
        dat <- tidyr::drop_na(tb())
        select_id <- (filter(building, ID == select()))$DAUID
        if (length(select_id) == 0) select_id <- NA
          
      } else {
        dat <- tidyr::drop_na(x())
        select_id <- select()
      }
      
      left_var_num <- length(unique(dat$left_var))
      bin_number <- min(25, left_var_num)
      var_left_title <- sus_translate(var_exp %>%
          filter(var_code == sub("_\\d{4}$", "", var_left())) %>%
          pull(var_name))
      var_right_title <- sus_translate(var_exp %>%
          filter(var_code == sub("_\\d{4}$", "", var_right())) %>%
          pull(var_name))
      na_select <- nrow(filter(dat, ID == select_id, !is.na(left_var_q3)))
      
      
      # Set up plotting variables ----------------------------------------------
      
      # Decide on plot type
      if (plot_type == "auto") {
        graph_type <- case_when(
          var_right() == " " & left_var_num > 6 ~ "hist",
          var_right() == " " & left_var_num <= 6 ~ "bar",
          var_right() != " " & left_var_num > 6 ~ "scatter",
          var_right() != " " & left_var_num <= 6 ~ "box")
        
        select_type <- case_when(is.na(select_id) ~ "all", 
                                 na_select == 0 ~ "na",
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
      v_left_title <- 
      labs_x <- list(labs(x = var_left_title, y = NULL))
      labs_xy <- list(labs(x = var_left_title, y = var_right_title))
      
      # Prepare default theme
      theme_default <- list(
        theme_minimal(),
        theme(legend.position = "none", panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()))
      
      
      # Render plot ------------------------------------------------------------
      
      # Histogram, no selection
      if (plot_type == "hist_all") {
        out <- ggplot(dat, aes(left_var)) +
          geom_histogram(aes(fill = fill), bins = bin_number) +
          scale_fill_manual(values = colour_scale[3:1], na.translate = FALSE) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Histogram, NA selection
      if (plot_type == "hist_na") {
        out <- ggplot(dat, aes(left_var)) +
          geom_histogram(bins = bin_number, fill = colour_scale[1]) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Histogram, active selection
      if (plot_type == "hist_select") {
        out <- ggplot(dat, aes(left_var)) +
          geom_histogram(aes(fill = round(left_var) == 
                               round(left_var[ID == select_id])),
                         bins = bin_number) +
          scale_fill_manual(values = colour_scale[c(1, 3)], 
                            na.translate = FALSE) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Bar, no selection
      if (plot_type == "bar_all") {
        out <- ggplot(dat, aes(as.factor(left_var))) +
          geom_bar(aes(fill = fill), width = 1) +
          scale_fill_manual(values = colour_scale[3:1], na.translate = FALSE) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Bar, NA selection
      if (plot_type == "bar_na") {
        out <- ggplot(dat, aes(as.factor(left_var))) +
          geom_bar(fill = colour_scale[1], width = 1) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Bar, active selection
      if (plot_type == "bar_select") {
        out <- ggplot(dat, aes(as.factor(left_var))) +
          geom_bar(aes(fill = round(left_var) == 
                               round(left_var[ID == select_id])), 
                         width = 1) +
          scale_fill_manual(values = colour_scale[c(1, 3)], 
                            na.translate = FALSE) +
          x_scale + y_scale + labs_x + theme_default
        }
      
      # Scatterplot, no selection
      if (plot_type == "scatter_all") {
        
        opac <- abs(cor(dat$left_var, dat$right_var, use = "complete.obs"))
        
        out <- ggplot(dat, aes(left_var, right_var)) +
          geom_point(aes(colour = group)) +
          stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                      formula = y ~ x, alpha = opac) +
          scale_colour_manual(values = tibble::deframe(colour_bivar)) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Scatterplot, NA selection
      if (plot_type == "scatter_na") {
        
        opac <- abs(cor(dat$left_var, dat$right_var, use = "complete.obs"))
        
        out <- ggplot(dat, aes(left_var, right_var)) +
          geom_point(colour = colour_bivar$fill[9]) +
          stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                      formula = y ~ x, alpha = opac) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Scatterplot, active selection
      if (plot_type == "scatter_select") {
        
        opac <- abs(cor(dat$left_var, dat$right_var, use = "complete.obs"))
        
        out <- ggplot(dat, aes(left_var, right_var)) +
          geom_point(colour = colour_bivar$fill[9]) +
          stat_smooth(geom = "line", se = FALSE, method = "loess", span = 1,
                      formula = y ~ x, alpha = opac) +
          geom_point(data = filter(dat, ID == select_id),
                     colour = colour_bivar$fill[1], size = 3) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Boxplot, no selection
      if (plot_type == "box_all") {
        
        colours <- c(colour_scale[1:2], rep(colour_scale[3], left_var_num - 2))
        names(colours) <- as.factor(unique(sort(dat$left_var)))
        
        out <- ggplot(dat, aes(as.factor(left_var), right_var)) +
          geom_boxplot(aes(fill = as.factor(left_var))) +
          scale_fill_manual(values = colours) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Boxplot, NA selection
      if (plot_type == "box_na") {
        
        colours <- c(colour_scale[1:2], rep(colour_scale[3], left_var_num - 2))
        names(colours) <- as.factor(unique(sort(dat$left_var)))
        
        out <- ggplot(dat, aes(as.factor(left_var), right_var)) +
          geom_boxplot(fill = colour_scale[1], colour = "grey50") +
          scale_fill_manual(values = colours) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      # Boxplot, active selection
      if (plot_type == "box_select") {
        
        colours <- c(colour_scale[1:2], rep(colour_scale[3], left_var_num - 2))
        names(colours) <- as.factor(unique(sort(dat$left_var)))
        
        out <- ggplot(dat, aes(as.factor(left_var), right_var)) +
          geom_boxplot(fill = colour_scale[1], colour = "grey50") +
          geom_point(data = filter(dat, ID == select_id),
                     colour = colour_bivar$fill[1], size = 4) +
          scale_fill_manual(values = colours) +
          x_scale + y_scale + labs_xy + theme_default
        }
      
      
      # Return output ----------------------------------------------------------
      
      return(out)
      
    }, bg = "white")
  })
}
