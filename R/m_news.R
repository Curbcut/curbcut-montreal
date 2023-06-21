### NEWS MODULE #####################################################


# Global functions --------------------------------------------------------

news <- get_news()

# News UI dynamic number of divs
news_divs <- function(id, info_list) {
  paste0(
    "susNewsExploreArticle(",
      "id = NS(id, '", id, "'),",
      "type = 'original',",
      "date = '", info_list$date, "',",
      "title = '", info_list$title, "',",
      "img = '", info_list$img, "',",
      "preview = '", info_list$preview, "')")
}

en_news_ui <- 
  mapply(news_divs, names(news$en), news$en,
         USE.NAMES = FALSE)

fr_news_ui <- 
  mapply(news_divs, names(news$fr), news$fr,
         USE.NAMES = FALSE)


# UI ----------------------------------------------------------------------

news_UI <- function(id) {
  tagList(
    susPage(class = "sus-page-news", footer = susFooter(),
            # KEEP HIDDEN ONLY UNTIL WE GET CONTROLS!
            hidden(div(id = NS(id, "controls"), 
            susPageControls(
                tags$div(
                  id = NS(id, "explore-controls"),
                  "Placeholder for explore view controls."
                ),
                tags$div(
                  id = NS(id, "news-controls"),
                  actionLink(NS(id, "back"), curbcut::cc_t(lang = r$lang(), "Back to explore"))
                )
              ))),
            susPageSection(
              
              tags$div(
                id = NS(id, "explore"),
                
                # English news
                tags$span(class = "lang-en",
                          eval(parse(text = en_news_ui))
                ),
                
                # French news
                tags$span(class = "lang-fr",
                          # FOR LATER, HOW TO DO A DYNAMIC NUMBER OF DIVS.
                          eval(parse(text = fr_news_ui))
                )
              ),
              
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
    
    # Create click observers on all the news blocks 
    news_click_observers <-
      sapply(c(names(news$en), names(news$fr)),
             \(x) paste0("observeEvent(input[['", x, 
                         "']], r[[id]]$select_id('", x, "'))"), 
             USE.NAMES = FALSE)
    eval(parse(text = news_click_observers))
    
    # Render the news in question
    output$news <- renderUI({
      if (!is.na(r[[id]]$select_id())) {
        
        news_link <- paste0("www/news/", r[[id]]$select_id(), ".html")
        
        HTML('<div class = "main_panel_text_popup">',
             includeHTML(news_link),
             '</div>')
      }
    })
    
    # Hide active news when "Go back to map" button is clicked
    observe(r[[id]]$select_id(NA)) |> bindEvent(input$back)
    
    observe({
      toggle("news-controls", condition = !is.na(r[[id]]$select_id()))
      toggle("news", condition = !is.na(r[[id]]$select_id()))
      toggle("explore-controls", condition = is.na(r[[id]]$select_id()))
      toggle("explore", condition = is.na(r[[id]]$select_id()))
      toggle("sus-page-controls", condition = is.na(r[[id]]$select_id()))
      toggle("controls", condition = !is.na(r[[id]]$select_id()))
    }) |> bindEvent(r[[id]]$select_id())
    
    # Bookmarking
    bookmark_server(
      id = id,
      r = r,
      s_id = r[[id]]$select_id
    )
    
  })
}
