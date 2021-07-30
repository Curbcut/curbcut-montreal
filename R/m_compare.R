#### COMPARE MODULE ############################################################

compare_UI <- function(id, var_list) {
  
  tagList(
    
    fluidRow(column(width = 7, h4(i18n$t("Compare"))),
             column(width = 5, align = "right", 
                    actionLink(inputId = NS(id, "hide"), 
                               label = i18n$t("Hide")))),
    
    conditionalPanel(
      condition = "output.hide_status == 1", ns = NS(id),
      select_var_UI(NS(id, "compare"), var_list),
      small_map_UI(NS(id, "right")))
    
  )
}

compare_server <- function(id, var_list, df, zoom = df) {
  stopifnot(!is.reactive(var_list))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(zoom))

  moduleServer(id, function(input, output, session) {
    
    # Select variable
    var_right <- select_var_server("compare", reactive(var_list))

    # Right map
    small_map_server("right", reactive(paste0(
      "right_", sub("_2", "", df()), "_", var_right())))

    # Hide compare status
    output$hide_status <- reactive(input$hide %% 2 == 0)
    outputOptions(output, "hide_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$hide, {
      if (input$hide %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Show")
      updateActionButton(session, "hide", label = txt)
    })
    
    reactive(var_right())
  })
}
