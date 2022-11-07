### WHY A DASHBOARD MODULE #####################################################

about_sus_UI <- function(id) {

  tagList(
    tags$head(tags$style(HTML(
      '#why_title_bar {border-width: 10px; border-color: rgb(255, 255, 255);}'))
    ),
    susPage(class = "sus-page-about", footer = susFooter(), susPageSection(
      h2(cc_t(r = r, "About Curbcut")),
      img(src = "glamour_shot.png", height = 300),
      htmlOutput(NS(id, "rmd_output"))
    )))
}


about_sus_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$rmd_output <- renderUI(
      # HTML(paste0(
        includeHTML(paste0("www/standalone/about_sus", "_", r$lang(), 
                           ".html"))#))
    )
    
  })
  }
