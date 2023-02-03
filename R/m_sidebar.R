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
                                      curbcut::cc_t(translation = translation, 
                                                    "Learn more"))),
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
    title <- modules[modules$id == id, ]
    title_title <- title$title_text_title
    title_main <- title$title_text_main
    title_extra <- title$title_text_extra
    
    # More info
    observeEvent(input$more_info, {
      toggle("title_extra", condition = input$more_info %% 2 == 1)
      txt <- curbcut::cc_t(lang = r$lang(), translation = translation, switch(input$more_info %% 2 + 1, "Learn more",
                                  "Hide"))
      updateActionLink(session, "more_info", label = txt)
      })
    
    
    output$title <- renderUI(h3(curbcut::cc_t(lang = r$lang(), translation = translation, title_title)))
    output$title_main <- renderUI(HTML(curbcut::cc_t(lang = r$lang(), translation = translation, title_main)))
    output$title_extra <- renderUI(HTML(curbcut::cc_t(lang = r$lang(), translation = translation, title_extra)))
    
  })
}
