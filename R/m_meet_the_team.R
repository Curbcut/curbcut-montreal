### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

meet_the_team_UI <- function(id) {
  ns <- NS(id)
  absolutePanel(
    h2(i18n$t("Meet the team")),
    uiOutput(outputId = ns("meet_the_team_html")),
    left = "250px",
    style = "max-height: 88vh; overflow-y: auto;"
  )
  
}


# Server ------------------------------------------------------------------

meet_the_team_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 
                 
                 output$meet_the_team_html <- renderUI({
                   
                   if (sus_reactive_variables$active_language() == "en") {
                     includeHTML("meet_the_team/meet_the_team_en.html")
                     
                   } else if (sus_reactive_variables$active_language() == "fr") {
                     includeHTML("meet_the_team/meet_the_team_fr.html")
                   }
                 })
                 
               })
  
}