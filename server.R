##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  observeEvent(input$title, {
    updateNavbarPage(session, "sus_page", "home")
  })
  
  # Language button ---------------------------------------------------------
  
  sus_rv$lang <- 
    eventReactive(input$language_button, {
      if (input$language_button[1] %% 2 != 0) "en" else "fr"
    }, ignoreNULL = FALSE)
  
  observeEvent(input$language_button,{
    if (input$language_button[1] %% 2 != 0) {
      update_lang(session, "en")
    } else {
      update_lang(session, "fr")
    }
  })
  
  
  # Modules -----------------------------------------------------------------
  
  home_server("home")
  
  active_mod_server <- function(active_tab = input$sus_page) {
    if (active_tab == "access") {
      access_server("access")
    } else if (active_tab == "alley") {
      alley_server("alley")    
    } else if (active_tab == "canale") {
      canale_server("canale")
    } else if (active_tab == "climate_risk") {
      climate_risk_server("climate_risk")
    } else if (active_tab == "covid") {
      covid_server("covid")
    } else if (active_tab == "crash") {
      crash_server("crash")
    } else if (active_tab == "gentrification") {
      gentrification_server("gentrification")
    } else if (active_tab == "housing") {
      housing_server("housing")
    } else if (active_tab == "mcp") {
      mcp_server("mcp")
    } else if (active_tab == "stories") {
      stories_server("stories")
    } else if (active_tab == "about") {
      why_dash_server("why_dash")
    }
  }
  
  observeEvent(input$sus_page, {
    active_mod_server()
  }, ignoreInit = TRUE)
  
  onRestore(function(state) {
    active_mod_server()
  })
  

  # Data download -----------------------------------------------------------
  
  # output$download_data <- 
  #   file_to_download()
    #DOWNLOAD HANDLER WITH A REACTIVE THAT'S CHANGING DEPENDING ON MODULE
  
  
  # Waiter ------------------------------------------------------------------
  
  waiter_hide()
  
})
