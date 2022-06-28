### NEWS MODULE #####################################################

news <- get_news_info()

# UI ----------------------------------------------------------------------

news_UI <- function(id) {
  tagList(
    susPage(class = "sus-page-news", footer = susFooter(), 
            susPageSection(
              
              # The back to general news button
              actionLink(NS(id, "back"), sus_translate(r = r, "Back to News")),
              
              # A first div, a block of news x
              div(id = NS(id, names(news)[1]),
                  class = "action-button shiny-bound-input",
                  fluidRow(column(9, h3(news[[1]]$title),
                  h5(news[[1]]$date),
                  h4(news[[1]]$preview)),
                  column(3, img(src = news[[1]]$img, 
                                align = "right")))),
              
              # A news that pops up
              hidden(htmlOutput(
                NS(id, "news")))
            )
    )
  )
}


# Server ------------------------------------------------------------------

news_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input[["welcome"]],
                 r[[id]]$select_id("welcome"))
    
    # Render the news in question
    output$news <- renderUI({
      if (!is.na(r[[id]]$select_id())) {
        
        news_link <- paste0("www/news/", r[[id]]$select_id(), "_", 
                            r$lang(), ".html")
        
        HTML('<div class = "main_panel_text_popup">',
             includeHTML(news_link),
             '</div>')
      }
    })
    # Hide active news when "Go back to map" button is clicked
    observe(r[[id]]$select_id(NA)) |> bindEvent(input$back)
    
    observe({
      toggle("back", condition = !is.na(r[[id]]$select_id()))
      toggle("news", condition = !is.na(r[[id]]$select_id()))
    }) |> bindEvent(r[[id]]$select_id())
    
  })
}
