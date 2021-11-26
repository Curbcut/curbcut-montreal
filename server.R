##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
    observeEvent(input$title, {
      updateNavbarPage(session, "sus_page", "home")
    })
  
  # Language button ---------------------------------------------------------
  
  sus_reactive_variables$active_language <- 
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
  
  observeEvent(input$sus_page, {
    if (input$sus_page == "access") {
      access_server("access")
    } else if (input$sus_page == "alley") {
      alley_server("alley")    
    } else if (input$sus_page == "canale") {
      canale_server("canale")
    } else if (input$sus_page == "climate_risk") {
      climate_risk_server("climate_risk")
    } else if (input$sus_page == "covid") {
      covid_server("covid")
    } else if (input$sus_page == "crash") {
      crash_server("crash")
    } else if (input$sus_page == "gentrification") {
      gentrification_server("gentrification")
    } else if (input$sus_page == "housing") {
      housing_server("housing")
    } else if (input$sus_page == "mcp") {
      mcp_server("mcp")
    } else if (input$sus_page == "stories") {
      stories_server("stories")
    } else if (input$sus_page == "about") {
      why_dash_server("why_dash")
    }
  }, ignoreInit = TRUE)
  

  # Restore the active tab when a session is restored with the bookmark -----
  
  onRestore(function(state) {
    if (input$sus_page == "access") {
      access_server("access")
    } else if (input$sus_page == "alley") {
      alley_server("alley")    
    } else if (input$sus_page == "canale") {
      canale_server("canale")
    } else if (input$sus_page == "climate_risk") {
      climate_risk_server("climate_risk")
    } else if (input$sus_page == "covid") {
      covid_server("covid")
    } else if (input$sus_page == "crash") {
      crash_server("crash")
    } else if (input$sus_page == "gentrification") {
      gentrification_server("gentrification")
    } else if (input$sus_page == "housing") {
      housing_server("housing")
    } else if (input$sus_page == "mcp") {
      mcp_server("mcp")
    } else if (input$sus_page == "stories") {
      stories_server("stories")
    } else if (input$sus_page == "about") {
      why_dash_server("why_dash")
    }
  })
  
    
  # Waiter ------------------------------------------------------------------
  
  waiter_hide()

})
