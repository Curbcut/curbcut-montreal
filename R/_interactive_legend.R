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
