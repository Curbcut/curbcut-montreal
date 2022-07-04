### HOW TO USE MODULE ##########################################################

how_to_use_UI <- function(id) {

  tagList(
    tags$head(tags$style(HTML(
      '#why_title_bar {border-width: 10px; border-color: rgb(255, 255, 255);}'))
    ),
    susPage(class = "sus-page-about", footer = susFooter(),susPageSection(
      htmlOutput(NS(id, "rmd_output"))
    )))
}


how_to_use_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$rmd_output <- renderUI(
      HTML(paste0(
      includeHTML(paste0("www/standalone/how_to_use", "_", r$lang(), 
                              ".html"))))
    )
    
  })
}
