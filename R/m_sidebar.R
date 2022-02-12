#### SIDEBAR MODULE ############################################################

sidebar_UI <- function(id, ...) {
  
  div(
    id = "title_bar", class = "sus-map-sidebar", 
    div(class = "sus-map-sidebar-content",
        tagList(
          uiOutput(NS(id, "title")),
          uiOutput(NS(id, "title_main")),
          actionLink(NS(id, "more_info"), class="sus-small-link", sus_translate("Learn more")),
          hidden(uiOutput(outputId = NS(id, "title_extra")))),
    ...
  ))
}

sidebar_server <- function(id, x) {
  stopifnot(!is.reactive(x))
  
  moduleServer(id, function(input, output, session) {
    
    # Prepare text
    title <- filter(title_text, tab == x)
    title_title <- if (nrow(title) == 0) "MISSING" else 
      (filter(title, type == "title"))$text
    title_main <- if (nrow(title) == 0) "MISSING" else 
      (filter(title, type == "main"))$text
    title_extra <- if (nrow(title) == 0) "MISSING" else 
      (filter(title, type == "extra"))$text
    
    # More info
    observeEvent(input$more_info, {
      toggle("title_extra", condition = input$more_info %% 2 == 1)
      if (input$more_info %% 2 == 1) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Learn more")
      updateActionLink(session, "more_info", label = txt)
      })
    
    output$title <- renderUI(h3(sus_translate(title_title)))
    output$title_main <- renderUI(HTML(sus_translate(title_main)))
    output$title_extra <- renderUI(HTML(sus_translate(title_extra)))
    
  })
}
