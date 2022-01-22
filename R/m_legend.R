#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  div(id = "legend", h5("Legend", style = "font-size: 12px;"),
      uiOutput(NS(id, "legend_render")))
}

legend_server <- function(id, var_left, var_right, df) {
  
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(df))

  moduleServer(id, function(input, output, session) {
    
    # Define plot height
    plot_height <- function() {
      # if (length(var_left()) == 1 && var_right()[1] == " ") 1 else 2.5
      if ((length(var_left()) == 1 && var_right()[1] == " ") ||
          (length(var_left()) == 2 && var_right()[1] == " ")) 60 else 140
    }
    
    # Get data type
    data_type <- reactive(get_data_type(df(), var_left(), var_right()))
    
    # Make legend
    legend <- reactive(render_legend(var_left(), var_right(), df(), 
                                     data_type()))
    
    # Output legend
    output$legend_render <- renderUI({
      output$legend <- renderPlot(legend())
      # Weird hack to get legend plot to inherit full namespace
      plotOutput(session$ns("legend"), height = plot_height(), width = "100%")
    })
    
    # Toggle legend display
    observe(toggle("legend", condition = !is.null(legend())))
    
    return(NULL)
    
  })
}
