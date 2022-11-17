#### COMPARE MODULE ############################################################

compare_UI <- function(id, var_list) {
  
  tagList(
    
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      fluidRow(column(width = 7, h4(cc_t(r = r, "Compare"))),
               column(width = 5, align = "right", 
                      actionLink(inputId = NS(id, "hide_compare"), 
                                 class = "sus-small-link",
                                 label = cc_t(r = r, "Hide"))))),
    
    conditionalPanel(
      condition = "output.hide_status == 1", ns = NS(id),
      div(id = NS(id, "compare"), 
          auto_vars_UI(NS(id, "compare"), var_list = var_list))),
    
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
    
    var_right <- auto_vars_server("compare", r = r,
                                  module_id = id,
                                  var_list = var_list,
                                  df = df,
                                  time)

    # Hide compare status
    output$show_panel <- show_panel
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
    
    output$hide_status <- reactive(show_panel() && input$hide_compare %% 2 == 0)
    outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$hide_compare, {
      if (input$hide_compare %% 2 == 0) {
        txt <- cc_t(r = r, "Hide")
      } else txt <- cc_t(r = r, "Show")
      updateActionButton(session, "hide_compare", label = txt)
    })
    
    var_right
  })
}
