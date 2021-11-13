#### COMPARE MODULE ############################################################

compare_UI <- function(id, var_list) {
  
  tagList(
    
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      fluidRow(column(width = 7, h4(i18n$t("Compare"))),
               column(width = 5, align = "right", 
                      actionLink(inputId = NS(id, "hide"), 
                                 label = i18n$t("Hide"))))),
    
    conditionalPanel(
      condition = "output.hide_status == 1", ns = NS(id),
      select_var_UI(NS(id, "compare"), var_list),
      small_map_UI(NS(id, "right"))),
    
    conditionalPanel(
      condition = "output.show_panel == true", ns = NS(id),
      hr())
  )
}

compare_server <- function(id, var_list, df, zoom = df, disabled_choices = NULL,
                           time = reactive(NULL), show_panel = reactive(TRUE)) {
  stopifnot(!is.reactive(var_list))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(zoom))
  stopifnot(is.reactive(show_panel))

  moduleServer(id, function(input, output, session) {
    
    var_right <- select_var_server("compare", reactive(var_list), 
                                   disabled = disabled_choices, time = time,
                                   df = df())

    # Right map
    small_map_server("right", reactive(paste0(
      "right_", df(), "_", var_right()[length(var_right())])))

    # Hide compare status
    output$show_panel <- show_panel
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
    
    output$hide_status <- reactive(show_panel() && input$hide %% 2 == 0)
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
