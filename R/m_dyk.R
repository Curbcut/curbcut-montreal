#### DID YOU KNOW MODULE ######################################################

dyk_hide_status <- reactive(TRUE)

dyk_UI <- function(id) {
  tagList(uiOutput(NS(id, "dyk_box")),
          uiOutput(NS(id, "dyk_contents")))
  }

dyk_server <- function(id, var_left, var_right) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  
  moduleServer(id, function(input, output, session) {
    
    dyk_output <- reactive(get_dyk_table(var_left(), var_right()))
    
    # Only show box if dyk_output isn't empty
    output$dyk_box <- renderUI({
      
      if (!is.null(dyk_output())) {
        tagList(
          hr(),
          fluidRow(column(width = 7, h4(sus_translate("Did you know?"))),
                   column(width = 5, align = "right",
                          actionLink(inputId = session$ns("hide"), class="sus-small-link",
                                     label = sus_translate("Hide"))))
        )
      }  
    })

    # Track hide status with clicks on input$hide button
    dyk_hide_status <- reactive(as.logical(input$hide %% 2 == 0))
    
    # Change show/hide button text
    observeEvent(dyk_hide_status(), {
      txt <- sus_translate(switch(input$hide %% 2 + 1, "Hide", "Show"))
      updateActionButton(session, "hide", label = txt)
    }, ignoreInit = TRUE)
    
    # Only show contents if dyk_output isn't empty and !dyk_hide_status
    output$dyk_contents <- renderUI({
      if (!is.null(dyk_output()) && req(dyk_hide_status())) {
        tagList(dyk_output())
      }
    })
      
  })
}
