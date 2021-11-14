### CRASH ANALYSIS MODULE ######################################################

# UI ----------------------------------------------------------------------

crash_analysis_UI <- function(id) {
  tabItem(tabName = "crash_analysis", 
          actionLink(NS(id, "crash_map"), label = "Map"),
          htmlOutput(NS(id, "crash_analysis_output"),
                     style = "max-width: 1000px;"))
}


# Server ------------------------------------------------------------------

crash_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$crash_map, {
      if (input$crash_map %% 2 != 0) {
        shinydashboard::updateTabItems(session, "tabs", "crash")
      }
      })
    
    output$crash_analysis_output <- renderUI(includeHTML(
      "www/crash/crash.html"))
    
  })
}
