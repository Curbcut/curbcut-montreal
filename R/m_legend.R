#### LEGEND MODULE #############################################################

legend_UI <- function(id) {
  tagList(
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      h5("Legend", style = "font-size: 12px;"),
      uiOutput(NS(id, "legend_render"))
      # ggiraph::girafeOutput(NS(id, "legend"), width = "100%")
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
    
    # Define plot height
    plot_height <- function() {
      if (length(var_left()) == 1 && var_right()[1] == " ") 60 else 140
    }
    
    output$legend_render <- renderUI({
      output$legend <- renderPlot({
        
        data_type <- get_data_type(df, var_left, var_right)
        tryCatch(render_plot_fun(var_left, var_right, df, data_type),
                 error = function(e) NULL, silent = TRUE)
      })
      # Weird hack to get legend plot to inherit full namespace
      plotOutput(session$ns("legend"), height = plot_height(), width = "100%")
    })

    # return(legend_display)
    return(NULL)
  })
}
