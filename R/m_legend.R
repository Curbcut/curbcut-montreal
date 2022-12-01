#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  div(id = NS(id, "legend_div"), h5(cc_t(r = r, "Legend"), 
                                    style = "font-size: 12px;"),
      uiOutput(NS(id, "legend_render")))
}

legend_server <- function(id, r, data, var_left, var_right, geo,
                          df = r[[id]]$df, hide = reactive(FALSE), 
                          build_str_as_DA = reactive(TRUE),
                          breaks = reactive(NULL)) {
  
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(build_str_as_DA))
  stopifnot(is.reactive(hide))
  
  moduleServer(id, function(input, output, session) {
    
    # Define plot height
    plot_height <- function() {
      # if (length(var_left()) == 1 && var_right()[1] == " ") 1 else 2.5
      if ((length(var_left()) == 1 && var_right()[1] == " ") ||
          (length(var_left()) == 2 && var_right()[1] == " ")) 60 else 150
    }
    
    # Get data type
    data_type <- reactive(tryCatch(
      get_data_type(
        df = df(), 
        var_left = var_left(), 
        var_right = var_right(),
        build_str_as_DA = build_str_as_DA()), 
      error = function(e) NULL))
    
    # Make legend
    legend <- reactive(tryCatch(
      render_legend(
        r = r,
        data = data(),
        var_left = var_left(),
        var_right = var_right(),
        df = df(),
        geo = geo(),
        data_type = data_type(),
        build_str_as_DA = build_str_as_DA(),
        breaks = breaks()),
      error = function(e) NULL)
    )

    # Output legend
    output$legend_render <- renderUI({
      output$legend <- renderPlot(legend())
      # Weird hack to get legend plot to inherit full namespace
      plotOutput(session$ns("legend"), height = plot_height(), width = "100%")
    })
    
    # Toggle legend display
    observe(toggle("legend_div", condition = !is.null(legend())))
    
    return(NULL)
    
  })
}
