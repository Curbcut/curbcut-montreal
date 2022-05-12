#### COMPARE MODULE ############################################################

#' @param df This only needs to be supplied to get the `return_closest_year`
#' feature. If that isn't needed, leave df NULL to avoid unneccessary reactivity
#' updates.

compare_UI <- function(id, var_list) {
  
  tagList(
    
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      fluidRow(column(width = 7, h4(sus_translate(r = r, "Compare"))),
               column(width = 5, align = "right", 
                      actionLink(inputId = NS(id, "hide_compare"), 
                                 class = "sus-small-link",
                                 label = sus_translate(r = r, "Hide"))))),
    
    conditionalPanel(
      condition = "output.hide_status == 1", ns = NS(id),
      div(class = "compare_dropdown",
          select_var_UI(NS(id, "compare"), var_list = var_list, inline = FALSE,
                    more_style = "width:100%;"))),
    
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      hr())
  )
}

compare_server <- function(id, r = r, var_list, df = r[[id]]$df, 
                           disabled = reactive(NULL), 
                           time = reactive(NULL), 
                           show_panel = reactive(TRUE)) {
  
  stopifnot(!is.reactive(var_list))
  stopifnot(is.reactive(disabled))
  stopifnot(is.reactive(time))
  stopifnot(is.reactive(show_panel))

  moduleServer(id, function(input, output, session) {
    
    var_right <- select_var_server("compare", r = r, var_list = reactive(var_list), 
                                   disabled = disabled, time = time, df = df)
    
    # Hide compare status
    output$show_panel <- show_panel
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
    
    output$hide_status <- reactive(show_panel() && input$hide_compare %% 2 == 0)
    outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$hide_compare, {
      if (input$hide_compare %% 2 == 0) {
        txt <- sus_translate(r = r, "Hide")
      } else txt <- sus_translate(r = r, "Show")
      updateActionButton(session, "hide_compare", label = txt)
    })
    
    reactive(var_right())
  })
}
