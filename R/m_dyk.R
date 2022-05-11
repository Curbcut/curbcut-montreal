#### DID YOU KNOW MODULE ######################################################

dyk_hide_status <- reactive(TRUE)

dyk_UI <- function(id) {
  tagList(uiOutput(NS(id, "dyk_box")),
          uiOutput(NS(id, "dyk_contents")))
  }

dyk_server <- function(id, r = r, var_left, var_right, df, 
                       poi = reactive(NULL)) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  
  moduleServer(id, function(input, output, session) {
    
    dyk_output <- reactive(get_dyk_table(id, r, var_left(), var_right(), 
                                         isolate(df()), poi()))
    
    # Observe for clicks
    observe(do.call(
      module_link, c(r = list(r), attr(dyk_output(), "links")[[1]]))) |> 
      bindEvent(input$dyk_1)
    
    observe(do.call(
      module_link, c(r = list(r), attr(dyk_output(), "links")[[2]]))) |> 
      bindEvent(input$dyk_2)
    
    # Only show box if dyk_output isn't empty
    output$dyk_box <- renderUI({
      if (!is.null(dyk_output())) {
        tagList(
          hr(),
          fluidRow(column(width = 7, h4(sus_translate(r = r, "Did you know?"))),
                   column(width = 5, align = "right",
                          actionLink(inputId = session$ns("hide_dyk"), 
                                     class = "sus-small-link",
                                     label = sus_translate(r = r, "Hide")))))
      }
    })

    # Track hide status with clicks on input$hide button
    dyk_hide_status <- reactive(as.logical(input$hide_dyk %% 2 == 0))
    
    # Change show/hide button text
    observeEvent(dyk_hide_status(), {
      txt <- sus_translate(r = r, switch(input$hide_dyk %% 2 + 1, "Hide", "Show"))
      updateActionButton(session, "hide_dyk", label = txt)
    }, ignoreInit = TRUE)
    
    # Only show contents if dyk_output isn't empty and !dyk_hide_status
    output$dyk_contents <- renderUI({
      if (!is.null(dyk_output()) && req(dyk_hide_status())) {
        tagList(dyk_output())
      }
    })
      
  })
}
