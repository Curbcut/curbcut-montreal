##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  ## Home page -----------------------------------------------------------------
  
  observe(updateNavbarPage(session, "sus_page", "home")) |> 
    bindEvent(input$title)
  

  # First visit banner ---------------------------------------------------------
  
  # Reset after 14 days of last time the banner was shown
  observeEvent(input$cookies$time_last_htu_banner, {
    if (is.null(input$cookies$time_last_htu_banner) ||
        (!is.null(input$cookies$time_last_htu_banner) &&
         Sys.time() > (as.POSIXct(input$cookies$time_last_htu_banner) + 1))) { #1209600))) {
      
      #TKTK SHOW BANNER HERE
      insertUI(selector = ".navbar-shadow",
               where = "beforeBegin",
               ui = HTML(paste0("<div id = 'htu_footer' class='fixed_footer'>",
                                "<p style = 'margin-bottom:0px; color:white; display:inline;'>",
                                "Première fois sur Sus? Visitez la page ",
                                paste0("<a id='go_to_htu_fr' href='#' style = 'color:white;'",
                                       "class='action-button shiny-bound-input'>",
                                       "<b>", "Mode d'emploi", 
                                       "</b></a> !&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"),
                                "First time on Sus? Visit the ",
                                paste0("<a id='go_to_htu_en' href='#' style = 'color:white;'",
                                       "class='action-button shiny-bound-input'>",
                                       "<b>", "How to use", 
                                       "</b></a> page!"), "</p>",
                                "<a id='go_to_htu_x' href='#' style = 'float:right;display:inline;color",
                                ":#FBFBFB;' class='action-button shiny-bound-input'>X</a>","</div>")))
    }
    
    # So that it repeats if there's a gap of 7 days between all visits
    time_last_htu_banner <- list(name = "time_last_htu_banner", 
                                 value = Sys.time())
    session$sendCustomMessage("cookie-set", time_last_htu_banner)
  }, once = TRUE)
  
  observeEvent(input$go_to_htu_en, {
    removeUI("#htu_footer")
    updateTabsetPanel(session, "sus_page", selected = "how_to_use")
  })
  
  observeEvent(input$go_to_htu_fr, {
    removeUI("#htu_footer")
    updateTabsetPanel(session, "sus_page", selected = "how_to_use")
  })
  
  observeEvent(input$go_to_htu_x, {
    removeUI("#htu_footer")
  })
  observeEvent(input$sus_page, {
    removeUI("#htu_footer")
  }, ignoreInit = TRUE)
  
  ## Language button -----------------------------------------------------------
  
  # Language reactive variable, JS set language, and language cookie. 
  # The three onclick of the language button.
  
  observeEvent(input$cookies$lang, {
    if (!is.null(input$cookies$lang) && input$cookies$lang == "en")
      click("language_button")
    # COOKIE, runs only once at launch !
  }, once = TRUE)
  
  sus_rv$lang <- 
    eventReactive(input$language_button, {
      if (input$language_button[1] %% 2 != 0) "en" else "fr"
    }, ignoreNULL = FALSE)
  
  observeEvent(input$language_button,{
    if (input$language_button[1] %% 2 != 0) {
      js$setLanguage("en")
      updateActionLink(inputId = "language_button", 
                       label = languageButtonLabel("Français"))
      
      lang_cookie <- list(name = "lang", value = "en")
      session$sendCustomMessage("cookie-set", lang_cookie)
      
    } else {
      js$setLanguage("fr")
      updateActionLink(inputId = "language_button", 
                       label = languageButtonLabel("English"))
      
      lang_cookie <- list(name = "lang", value = "fr")
      session$sendCustomMessage("cookie-set", lang_cookie)
    }
  })
  

  ## Active tab ----------------------------------------------------------------
  
  sus_rv$active_tab <-
    eventReactive(input$sus_page, input$sus_page, ignoreNULL = FALSE)
  
  observeEvent(input$sus_page, {
    sus_rv$last_module <- unique(c(sus_rv$current_module, sus_rv$last_module))
    sus_rv$current_module <- c(input$sus_page, sus_rv$last_module)})
  
  sus_rv$previous_tabs <- reactive({
    req(input$sus_page)
    input$sus_page
    sus_rv$last_module})

  observeEvent(sus_rv$link, {
    # Switch active tab when link is opened
    updateTabsetPanel(session, "sus_page", selected = sus_rv$link())
    # Turn off the link
    sus_rv$link <- NULL
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Links between modules
  observeEvent(sus_link$activity, {
      # Delay to make sure the linked module is fully loaded
      delay(500, {
        update_module(mod_ns = sus_link$mod_ns, 
                      session = session, 
                      zoom = sus_link$zoom, 
                      location = sus_link$location, 
                      map_id = sus_link$map_id,
                      df = sus_link$df, 
                      zoom_auto = sus_link$zoom_auto, 
                      var_left = sus_link$var_left,
                      var_right = sus_link$var_right, 
                      more_args = sus_link$more_args)
      })
  }, ignoreInit = TRUE)
  

  ## Parse URL -----------------------------------------------------------------
  
  sus_bookmark$active <- FALSE
  
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (length(query) != 0) {
      
      # MARK THE ACTIVE BOOKMARKED
      sus_bookmark$active <- TRUE
      
      # query returns a named list. If it's named tab, return the right
      # tabPanel. the url example: sus.ca/?tab=housing
      try({
        tab <- query[["tb"]]
        if (!is.null(tab))
          updateTabsetPanel(session, "sus_page", selected = query[["tb"]])
      })
      # Update language with query.
      try({
        lang <- query[["lng"]]
        if (!is.null(lang) && lang == "en")
          click("language_button")
      })
      # Retrieve important map info
      try({
        if (!is.null(query[["zm"]])) {
        sus_bookmark$zoom <- as.numeric(query[["zm"]])}
        
        if (!is.null(query[["lon"]])) {
        sus_bookmark$location <- c(as.numeric(query[["lon"]]), 
                                   as.numeric(query[["lat"]]))}
      })
      # Retrieve var_left
      try({
        if (!is.null(query[["v_l"]]))
          sus_bookmark$var_left <- query[["v_l"]]
      })
      # Retrieve var_right
      try({
        if (!is.null(query[["v_r"]]))
        sus_bookmark$var_right <- query[["v_r"]]
      })
      # Retrieve select_id
      try({
        if (!is.null(query[["s_id"]]))
        sus_bookmark$select_id <- query[["s_id"]]
      })
      # Retrieve if df is manual
      try({
        if (!is.null(query[["zm_a"]])) {
        sus_bookmark$zoom_auto <- as.logical(query[["zm_a"]])
        sus_bookmark$df <- query[["df"]]}
      })
      try({
        if (!is.null(query[["more"]]))
        sus_bookmark$more_args <- query[["more"]]
      })
    }
  })
  
  
  ## Modules -------------------------------------------------------------------
  
  active_mod_server <- function(active_tab = input$sus_page) {
    if (active_tab == "home") return(home_server("home", session))
    if (active_tab == "access") return(access_server("access"))
    if (active_tab == "alley") return(alley_server("alley"))    
    if (active_tab == "canale") return(active_mod <<- canale_server("canale"))
    if (active_tab == "climate_risk") 
      return(climate_risk_server("climate_risk"))
    if (active_tab == "covid") return(covid_server("covid"))
    if (active_tab == "crash") return(crash_server("crash"))
    if (active_tab == "gentrification") 
      return(active_mod <<- gentrification_server("gentrification"))
    if (active_tab == "green_space") return(green_space_server("green_space"))
    if (active_tab == "housing") return(housing_server("housing"))
    if (active_tab == "marketed_sustainability") 
      return(marketed_sustainability_server("marketed_sustainability"))
    if (active_tab == "natural_inf") 
      return(natural_inf_server("natural_inf"))
    if (active_tab == "mcp") return(mcp_server("mcp"))
    if (active_tab == "permits") return(permits_server("permits"))
    if (active_tab == "stories") return(stories_server("stories"))
    if (active_tab == "place_explorer") 
      return(place_explorer_server("place_explorer"))
    if (active_tab == "about") return(why_dash_server("why_dash"))
    if (active_tab == "how_to_use") return(how_to_use_server("how_to_use"))
  }
  
  observeEvent(input$sus_page, {
    bookmark_server("reset")
    if (!is.null(sus_bookmark$active) && isTRUE(sus_bookmark$active)) 
      active_mod_server()
    if (!input$sus_page %in% sus_rv$previous_tabs()) active_mod_server()
    updateQueryString("?")
  }, ignoreInit = FALSE)
  
  
  ## Data download -------------------------------------------------------------
  
  dataModal <- function() {
    modalDialog(
      "PLACEHOLDER",
      
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_csv", "Download csv"),
        downloadButton("download_shp", "Download shp")
      ),
      title = "Data explanation and export"
    )
  }
  
  onclick("download_data", {
    showModal(
      dataModal()
    )
  })
  
  output$download_csv <-
    downloadHandler(
      filename = paste0(active_mod()$module_id, "_data.csv"),
      content = function(file) {
        data <- active_mod()$data
        write.csv2(data, file)
        },
      contentType = "text/csv")
  
  output$download_shp <-
    downloadHandler(
      filename = paste0(active_mod()$module_id, "_shp.zip"),
      content = function(file) {
        withProgress(message = "Exporting Data", {
          incProgress(0.5)
          tmp.path <- dirname(file)
          name.base <- file.path(tmp.path, 
                                 paste0(active_mod()$module_id, "_data"))
          name.glob <- paste0(name.base, ".*")
          name.shp  <- paste0(name.base, ".shp")
          name.zip  <- paste0(name.base, ".zip")
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
          sf::st_write(active_mod()$data, dsn = name.shp, 
                       driver = "ESRI Shapefile", quiet = TRUE)
          
          zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
          req(file.copy(name.zip, file))
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
          incProgress(0.5)
        })
      })
  
  
  ## Contact form --------------------------------------------------------------
  
  contactModal <- function() {
    modalDialog(
      selectInput("contact_type", "Reason for contact",
                  choices = c("Contact" = "CONTACT",
                              "Report a bug" = "BUG",
                              "Feedback" = "FEEDBACK",
                              "Other" = "OTHER"), width = "75%"),
      textInput("contact_from_name", "Your name/organization", width = "75%"),
      textInput("contact_from", "Your email adress", "@", width = "75%"),
      textInput("contact_subject", "Subject", width = "75%"),
      textAreaInput("contact_body", "Content", width = "75%", height = "300px"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("send_feedback", "Send")),
      title = "Contact form"
    )
  }
  
  onclick("contact", {
    showModal(
      contactModal()
    )
  })
  
  observeEvent(input$send_feedback, {
    # sendmailR::sendmail(from = paste0("<", input$contact_from, ">"),
    #                     to = "<maximebdeblois@gmail.com>",
    #                     subject = paste0(input$contact_type, " - ", 
    #                                     input$contact_subject),
    #                     body = input$contact_body,
    #                     # This is the part not working atm:
    #                     control = list(smtpServer="smtp.gmail.com"))
    
    # Other possibility:
    contact_form <- c(name = input$contact_from_name,
                      email = input$contact_from,
                      subject = paste(input$contact_type, " - ", 
                                      input$contact_subject),
                      body = input$contact_body)
    time_stamp <- str_replace_all(Sys.time(), c(" |:"), "-")
    file_name <- paste0("contacts/",input$contact_type, "-", time_stamp, ".csv")
    write.csv2(contact_form, file = file_name)
    removeModal()
    showNotification(sus_translate("Sent and received. Thank you!"), 
                     duration = 3)
  })
  
  
  ## Generating report ---------------------------------------------------------
  
  output$create_report <-
    downloadHandler(
      filename = "report.html",
      content = function(file) {
        shiny::withProgress(
          message = sus_translate(paste0("Generating report on ",
                                         active_mod()$module_short_title)),
          {
            shiny::incProgress(0.35)
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
            params <- list(
              module_short_title = active_mod()$module_short_title,
              module = active_mod()$module_id,
              map_title = title_text$text[
                title_text$tab == active_mod()$module_id & 
                  title_text$type == "title"],
              time = active_mod()$time,
              data = active_mod()$data,
              token = active_mod()$token,
              map_zoom = active_mod()$map_zoom,
              map_loc = active_mod()$map_loc,
              df = active_mod()$df,
              explore_content = active_mod()$explore_content,
              poly_selected = active_mod()$poly_selected,
              legend_graph = active_mod()$legend_graph)
            shiny::incProgress(0.35)
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
            shiny::incProgress(0.3)
          })
        }
      )
  })
