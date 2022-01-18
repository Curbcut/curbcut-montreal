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
    
    # Define plot height
    plot_height <- function() {
      # if (length(var_left()) == 1 && var_right()[1] == " ") 1 else 2.5
      if ((length(var_left()) == 1 && var_right()[1] == " ") ||
          (length(var_left()) == 2 && var_right()[1] == " ")) 60 else 140
    }
    
    # # catch if there's an error in the legend function
    # legend_display <- reactive({
    #   data_type <- get_data_type(df, var_left, var_right)
    #   tryCatch(render_plot_fun(var_left, var_right, df, data_type),
    #            error = function(e) NULL, silent = TRUE)
    # })
    # 
    # output$legend_render <- renderUI({
    #   output$legend <- renderGirafe({
    #     # Only show legend if there's something to show
    #     if (!is.null(legend_display())) {
    #       girafe(ggobj = legend_display(),
    #              # Height is in inches
    #              height_svg = plot_height(),
    #              options = list(
    #                opts_hover_inv(css = "opacity:0.7"),
    #                opts_hover(css = "cursor:pointer;"),
    #                opts_selection(css = "fill:red;"),
    #                opts_toolbar(saveaspng = FALSE, position = "bottomleft"),
    #                opts_sizing(rescale = TRUE, width = 1)))
    #     }
    #   })
    #   # Weird hack to get legend plot to inherit full namespace
    #   girafeOutput(session$ns("legend"), height = plot_height()*60, 
    #                width = "100%")
    # })
    # 
    # reactive(list(legend_selection = input$legend_selected))
    
    output$legend_render <- renderUI({
      output$legend <- renderPlot({
        
        data_type <- get_data_type(df(), var_left(), var_right())
        
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
