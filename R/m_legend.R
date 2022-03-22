#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  div(id = "legend", h5(sus_translate("Legend"), style = "font-size: 12px;"),
      uiOutput(NS(id, "legend_render")))
}

legend_server <- function(id, data, var_left, var_right, df, 
                          build_str_as_DA = reactive(TRUE)) {
  
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(build_str_as_DA))
  
  moduleServer(id, function(input, output, session) {
    
    # Define plot height
    plot_height <- function() {
      # if (length(var_left()) == 1 && var_right()[1] == " ") 1 else 2.5
      if ((length(var_left()) == 1 && var_right()[1] == " ") ||
          (length(var_left()) == 2 && var_right()[1] == " ")) 60 else 140
    }
    
    # Get data type
    data_type <- reactive(get_data_type(df(), var_left(), var_right(),
                                        build_str_as_DA()))
    
    # Make legend
    legend <- reactive(render_legend(data(), var_left(), var_right(), df(), 
                                     data_type(), build_str_as_DA()))
    
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
