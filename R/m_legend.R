#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  tagList(
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      
      h5("Legend", style = "font-size: 12px;"),
      uiOutput(NS(id, "legend_render"))
    )
  )
}

legend_server <- function(id, var_left, var_right, df, 
                          show_panel = reactive(TRUE)) {
  
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    
    # Hide legend
    output$show_panel <- show_panel
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
    
    plot_height <- function() {
      if (length(var_left()) == 1 && var_right()[1] == " ") 60 else 120
    }
    
    render_plot_fun <- function() {
      
      ## One variable, one date, q5 --------------------------------------------
      
      if (length(var_left()) == 1 && var_right()[1] == " ") {
        
        date_left <- str_extract(var_left(), "(?<=_)\\d{4}$")
        
        axis_title <- sus_translate(
          variables |> 
            filter(var_code == unique(sub("_\\d{4}$", "", var_left()))) |> 
            pull(var_short))
        
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
        
        legend_left_5 |> 
          ggplot(aes(xmin = x - 1, xmax = x, ymin = y - 1, ymax = y, 
                     fill = fill)) +
          geom_rect() + 
          scale_x_continuous(name = axis_title, breaks = 0:5,
                             labels = as.character(break_labels)) +
          scale_y_continuous(name = NULL, labels = NULL) +
          scale_fill_manual(values = setNames(
            paste0(legend_left_5$fill, 
                   filter(colour_alpha, zoom == df())$alpha),
            legend_left_5$fill)) +
          theme_minimal() +
          theme(legend.position = "none",
                panel.grid = element_blank())
        
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
                panel.grid = element_blank())
        
      } else {
        
        var_left <- unique(str_remove(var_left(), "_\\d{4}$"))
        var_right <- unique(str_remove(var_right(), "_\\d{4}$"))
        
        var_left_title <- 
          variables |> 
          filter(var_code == var_left) |> 
          pull(var_title)
        
        var_right_title <- 
          variables |> 
          filter(var_code == var_right) |> 
          pull(var_title)
        
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
                    inherit.aes = FALSE, size = 3) +
          scale_fill_manual(values = setNames(
            paste0(legend_bivar$fill, 
                   filter(colour_alpha, zoom == df())$alpha),
            legend_bivar$fill)) +
          scale_colour_manual(values = c(
            "black" = "black", "white" = "white")) +
          labs(x = paste0(var_right_title, " (low to high)"), 
               y = paste0(var_left_title, "Housing (low to high)")) +
          theme_void() +
          theme(legend.position = "none")
        
      }
    }
    
    # catch if there's an error in the legend function
    legend_display <- reactive({
      tryCatch(render_plot_fun(),
               error = function(e) reactive(NULL),
               silent = TRUE)
    })
    
    output$legend_render <- renderUI({
      output$legend <- renderPlot({
        # Only show legend if there's something to show
        if (!is.null(legend_display())) legend_display()
      })
      
      # Weird hack to get legend plot to inherit full namespace
      plotOutput(session$ns("legend"), height = plot_height(), width = "100%")
      
    })
    
    reactive(render_plot_fun())
    
  })
}
