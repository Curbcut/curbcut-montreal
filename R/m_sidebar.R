#### SIDEBAR MODULE ############################################################

sidebar_UI <- function(id, ..., bottom = NULL) {
  return(tagList(
    div(class = "sus-map-sidebar-shadow"),
    div(
      id = "title_bar", class = "sus-map-sidebar", 
      div(class = "sus-map-sidebar-container",
          div(class = "sus-map-sidebar-content sus-scroll",
              div(class = "sus-scroll-content",
                  tagList(
                    uiOutput(NS(id, "title")),
                    tags$p(uiOutput(NS(id, "title_main"))),
                    tags$p(actionLink(NS(id, "more_info"), 
                                      class = "sus-small-link", 
                                      sus_translate(r = r, "Learn more"))),
                    hidden(uiOutput(outputId = NS(id, "title_extra"))),
                    ...
                  )
              )
          ),
          bottom
      )
    )
  ))
}

sidebar_server <- function(id, r, x = id) {
  stopifnot(!is.reactive(x))
  
  moduleServer(id, function(input, output, session) {
    
    # Prepare text
    title <- title_text[title_text$tab == x,]
    title_title <- if (nrow(title) == 0) "MISSING" else 
      title$text[title$type == "title"]
    title_main <- if (nrow(title) == 0) "MISSING" else 
      title$text[title$type == "main"]
    title_extra <- if (nrow(title) == 0) "MISSING" else 
      title$text[title$type == "extra"]
    
    # More info
    observeEvent(input$more_info, {
      toggle("title_extra", condition = input$more_info %% 2 == 1)
      txt <- sus_translate(r = r, switch(input$more_info %% 2 + 1, "Learn more", 
                                  "Hide"))
      updateActionLink(session, "more_info", label = txt)
      })
    
    output$title <- renderUI(h3(sus_translate(r = r, title_title)))
    output$title_main <- renderUI(HTML(sus_translate(r = r, title_main)))
    output$title_extra <- renderUI(HTML(sus_translate(r = r, title_extra)))
    
  })
}
