### NEWS MODULE #####################################################


# Global functions --------------------------------------------------------

news <- get_news()

# News UI dynamic number of divs
news_divs <- function(id, info_list) {
  # TODO: somehow using the function
  return (
    susNewsExploreArticle(
      id=id,
      type='original',
      author='Bob Loblaw', 
      date=info_list$date,
      title=info_list$title,
      img=info_list$img,
      preview=info_list$preview
    )
  )
  
  # paste0("div(id = NS(id, '", id, "'),",
  #        "class = 'action-button shiny-bound-input',",
  #        "fluidRow(column(9, tags$h1(", info_list, "$title),",
  #        "tags$span(", info_list, " $date),",
  #        "tags$p(", info_list, "$preview)),",
  #        "column(3, img(src = ", info_list, "$img, ",
  #        "align = 'right'))))")
}

en_news_ui <- 
  mapply(news_divs, names(news$en), list(news$en),
         USE.NAMES = FALSE)

fr_news_ui <- 
  mapply(news_divs, names(news$fr), list(news$fr),
         USE.NAMES = FALSE)


# UI ----------------------------------------------------------------------

news_UI <- function(id) {
  tagList(
    susPage(class = "sus-page-news", footer = susFooter(),
            susPageControls(
                tags$div(
                  id=NS(id, "explore-controls"),
                  tags$span(id=id, "Placeholder for explore view controls.")
                ),
                tags$div(
                  id=NS(id, "news-controls"),
                  actionLink(NS(id, "back"), sus_translate(r = r, "Back to explore"))
                )
              ),
            susPageSection(
              
              tags$div(
                id=NS(id, "explore"),
                
                # English news
                tags$span(class = "lang-en",
                          susNewsExploreArticle(
                            id=id,
                            type='original',
                            author='Bob Loblaw', 
                            date='2022/07/04',
                            title='Welcome to SUS, your daily platform for Montreal climate, transit, policy, and active living resources',
                            img='/www/news/visuals/welcome/sus_logo_256x256.png',
                            preview='Sus is an urban sustainability platform for the Montreal region. It operates across multiple spatial and temporal scales and offers a justice- and inclusivity-focused approach to sustainability.'
                          )
                          
                          # A first div, a block of news x
                          # div(id = NS(id, names(news$en)[1]),
                          #     class = "action-button shiny-bound-input",
                          #     fluidRow(column(9, tags$h1(news$en[[1]]$title),
                          #                     tags$span(news$en[[1]]$date),
                          #                     tags$p(news$en[[1]]$preview)),
                          #              column(3, img(src = news$en[[1]]$img, 
                          #                            align = "right"))))
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
    }) |> bindEvent(r[[id]]$select_id())
    
  })
}
