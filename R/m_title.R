#### TITLE BAR MODULE ##########################################################

title_UI <- function(id, ...) {

  absolutePanel(
    id = "title_bar", class = "panel panel-default", 
    style = "padding: 5px; margin: 0px 5px; border-width: 0px; z-index: 500",
    draggable = FALSE, top = 70, width = "40%", 
    uiOutput(NS(id, "title")),
    uiOutput(NS(id, "title_main")),
    ...,
    div(),
    actionLink(NS(id, "more_info"), i18n$t("Learn more")),
    conditionalPanel(id = NS(id, "cond"),
      condition = "output.more_info_status == 1", ns = NS(id),
      uiOutput(NS(id, "title_extra"))
    )
  )
}

title_server <- function(id, x) {
  stopifnot(!is.reactive(x))
  
  moduleServer(id, function(input, output, session) {
    
    title <- filter(title_text, tab == x)
    
    # More info
    output$more_info_status <- reactive(input$more_info %% 2 == 1)
    outputOptions(output, "more_info_status", suspendWhenHidden = FALSE)

    observe({
      if (input$more_info %% 2 == 1) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Learn more")
      updateActionLink(session, "more_info", label = txt)
      })
    
    output$title <- renderUI(h2(sus_translate(
      if (nrow(title) == 0) "MISSING" else 
        (filter(title, type == "title"))$text)))
    
    output$title_main <- renderUI(p(sus_translate(
      if (nrow(title) == 0) "MISSING" else 
        (filter(title, type == "main"))$text)))
    
    output$title_extra <- renderUI(HTML(sus_translate(
      if (nrow(title) == 0) "MISSING" else 
        (filter(title, type == "extra"))$text)))
  })
}
