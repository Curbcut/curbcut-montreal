##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  

  # Page title change, depending on page visited -------------------------------

  observe(title_page_update(r = r, session = session, 
                            sus_page = input$sus_page))
  
  
  # If on mobile, warning! -----------------------------------------------------
  
  observe(mobile_warning(r = r, session = session))
  
  
  # Reactive variables ---------------------------------------------------------
  
  r <- reactiveValues(
    sus_bookmark = reactiveValues(active = FALSE),
    sus_link = reactiveValues(),
    lang = reactiveVal("fr"),
    active_tab = "home",
    canale = reactiveValues(select_id = reactiveVal(NA), 
                            df = reactiveVal("borough"),
                            zoom = reactiveVal(get_zoom(map_zoom))),
    climate_risk = reactiveValues(select_id = reactiveVal(NA),
                                  df = reactiveVal("grid"),
                                  zoom = reactiveVal(get_zoom(map_zoom))),
    housing = reactiveValues(select_id = reactiveVal(NA),
                             df = reactiveVal("borough"),
                             zoom = reactiveVal(get_zoom(map_zoom))),
    access = reactiveValues(select_id = reactiveVal(NA),
                            df = reactiveVal("CT"),
                            zoom = reactiveVal(get_zoom(map_zoom))),
    alley = reactiveValues(select_id = reactiveVal(NA),
                           df = reactiveVal("borough_empty"),
                           zoom = reactiveVal(12)),
    natural_inf = reactiveValues(zoom = reactiveVal(9.5))
  )
  

  # Home page ------------------------------------------------------------------
  
  observe(updateNavbarPage(session, "sus_page", "home")) |> 
    bindEvent(input$title)
  
  
  # First visit banner ---------------------------------------------------------
  
  # Reset after 14 days of last time the banner was shown
  observeEvent(input$cookies$time_last_htu_banner, {
    if (is.null(input$cookies$time_last_htu_banner) ||
        (!is.null(input$cookies$time_last_htu_banner) &&
         Sys.time() > (as.POSIXct(input$cookies$time_last_htu_banner) + 
                       1209600))) {
      
      insertUI(selector = ".navbar-shadow", where = "beforeBegin", ui = HTML(
        paste0("<div id = 'htu_footer' class='fixed_footer'>",
               "<p style = 'margin-bottom:0px; color:white; display:inline;'>",
               "Première fois sur Sus? Visitez la page ",
               paste0("<a id='go_to_htu_fr' href='#' style = 'color:white;'",
                      "class='action-button shiny-bound-input'>",
                      "<b>", "Mode d'emploi", 
                      "</b></a> !&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"),
               "First time on Sus? Visit the ",
               paste0("<a id='go_to_htu_en' href='#' style = 'color:white;'",
                      "class='action-button shiny-bound-input'>",
                      "<b>", "How to use", "</b></a> page!"), "</p>",
               "<a id='go_to_htu_x' href='#' ",
               "style = 'float:right;display:inline;color",
               ":#FBFBFB;' class='action-button shiny-bound-input'>X</a>",
               "</div>")))
    }
    
    # So that it repeats if there's a gap of 14 days between all visits
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
  
  
  ## Language button -----------------------------------------------------------
  
  # Language reactive variable, JS set language, and language cookie. 
  # The three onclick of the language button.
  
  observeEvent(input$cookies$lang, {
    if (!is.null(input$cookies$lang) && input$cookies$lang == "en")
      r$lang("en")
  }, once = TRUE)
  
  observeEvent(input$language_button, {
    r$lang(if (input$language_button[1] %% 2 != 0) "en" else "fr")
  }, ignoreNULL = FALSE)
  
  observeEvent(r$lang(), {
    if (r$lang() == "en") {
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
  
  observeEvent(input$sus_page, r$active_tab <- input$sus_page, 
               ignoreNULL = FALSE)
  
  observeEvent(input$sus_page, {
    r$last_module <- unique(c(r$current_module, r$last_module))
    r$current_module <- c(input$sus_page, r$last_module)})
  
  r$previous_tabs <- reactive({
    req(input$sus_page)
    input$sus_page
    r$last_module})
  
  observeEvent(r$link, {
    # Switch active tab when link is opened
    updateTabsetPanel(session, "sus_page", selected = r$link)
    # Turn off the link
    r$link <- NULL
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Links between modules
  observeEvent(r$sus_link$activity, {
    # Delay to make sure the linked module is fully loaded
    delay(500, {
      update_module(id = r$sus_link$id,
                    r = r, 
                    map_id = "map",
                    session = session,
                    zoom = r$sus_link$zoom,
                    location = r$sus_link$location,
                    zoom_auto = r$sus_link$zoom_auto,
                    var_left = r$sus_link$var_left,
                    var_right = r$sus_link$var_right,
                    more_args = r$sus_link$more_args)
    })
  }, ignoreInit = TRUE)
  
  
  ## Parse URL -----------------------------------------------------------------
  
  observeEvent(parseQueryString(session$clientData$url_search), {
    query <- parseQueryString(session$clientData$url_search)
    
    if (length(query) != 0) {
      
      # MARK THE ACTIVE BOOKMARKED
      r$sus_bookmark$active <- TRUE
      
      # query returns a named list. If it's named tab, return the right
      # tabPanel. the url example: sus.ca/?tab=housing
      try({
        tab <- query[["tb"]]
        if (!is.null(tab))
          updateTabsetPanel(session, "sus_page", selected = query[["tb"]])
        r$sus_bookmark$id <- tab
      })
      # Update language with query.
      try({
        lang <- query[["lng"]]
        if (!is.null(lang) && lang == "en")
          r$lang <- reactiveVal("en")
      })
      # Retrieve important map info
      try({
        if (!is.null(query[["zm"]])) {
          r[[query[["tb"]]]]$zoom <- reactiveVal(as.numeric(query[["zm"]]))}
        
        if (!is.null(query[["lon"]])) {
          r$sus_bookmark$location <- c(as.numeric(query[["lon"]]), 
                                       as.numeric(query[["lat"]]))}
      })
      # Retrieve var_left
      try({
        if (!is.null(query[["v_l"]]))
          r$sus_bookmark$var_left <- query[["v_l"]]
      })
      # Retrieve var_right
      try({
        if (!is.null(query[["v_r"]]))
          r$sus_bookmark$var_right <- query[["v_r"]]
      })
      # Retrieve if zoom is automatic or not
      try({
        if (!is.null(query[["zm_a"]]))
          r$sus_bookmark$zoom_auto <- as.logical(query[["zm_a"]])
      })
      # Additional
      try({
        if (!is.null(query[["more"]]))
          r$sus_bookmark$more_args <- query[["more"]]
      })
      # Retrieve df
      try({
        if (!is.null(query[["df"]])) 
          r[[query[["tb"]]]]$df <- reactiveVal(query[["df"]])
      })
      # Retrieve select_id
      try({
        if (!is.null(query[["s_id"]])) 
          r[[query[["tb"]]]]$select_id <- reactiveVal(query[["s_id"]])
      })
    }
  }, once = TRUE)
  
  # Update from bookmark
  observeEvent(r$sus_bookmark$active, {
    if (isTRUE(r$sus_bookmark$active)) {
      # Delay to make sure the bookmarked module is fully loaded
      delay(500, {
        update_module(session = session,
                      r = r,
                      id = r$sus_bookmark$id,
                      map_id = "map",
                      location = r$sus_bookmark$location, 
                      zoom_auto = r$sus_bookmark$zoom_auto, 
                      var_left = r$sus_bookmark$var_left,
                      var_right = r$sus_bookmark$var_right, 
                      more_args = r$sus_bookmark$more_args)
      })
    }
  }, priority = -1, once = TRUE)
  
  
  ## Modules -------------------------------------------------------------------
  
  active_mod_server <- function(active_tab = input$sus_page) {
    mod_function <- 
      paste0(active_tab, "_server('", active_tab, "', r = r)")
    
    # Run the function but also catch its output for data exportation
    assign("export_data", eval(parse(text = mod_function)), 
           pos = 1)
  }
  
  observeEvent(input$sus_page, {
    bookmark_server(input$sus_page, r = r)

    # Trigger the module server function only if it hasn't been opened already
    if (!input$sus_page %in% r$previous_tabs()) active_mod_server()
    
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
  
  # contactModal <- function() {
  #   modalDialog(
  #     selectInput("contact_type", "Reason for contact",
  #                 choices = c("Contact" = "CONTACT",
  #                             "Report a bug" = "BUG",
  #                             "Feedback" = "FEEDBACK",
  #                             "Other" = "OTHER"), width = "75%"),
  #     textInput("contact_from_name", "Your name/organization", width = "75%"),
  #     textInput("contact_from", "Your email adress", "@", width = "75%"),
  #     textInput("contact_subject", "Subject", width = "75%"),
  #     textAreaInput("contact_body", "Content", width = "75%", height = "300px"),
  #     
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("send_feedback", "Send")),
  #     title = "Contact form"
  #   )
  # }
  # 
  # onclick("contact", {
  #   showModal(
  #     contactModal()
  #   )
  # })
  # 
  # observeEvent(input$send_feedback, {
  #   # sendmailR::sendmail(from = paste0("<", input$contact_from, ">"),
  #   #                     to = "<maximebdeblois@gmail.com>",
  #   #                     subject = paste0(input$contact_type, " - ", 
  #   #                                     input$contact_subject),
  #   #                     body = input$contact_body,
  #   #                     # This is the part not working atm:
  #   #                     control = list(smtpServer="smtp.gmail.com"))
  #   
  #   # Other possibility:
  #   contact_form <- c(name = input$contact_from_name,
  #                     email = input$contact_from,
  #                     subject = paste(input$contact_type, " - ", 
  #                                     input$contact_subject),
  #                     body = input$contact_body)
  #   time_stamp <- str_replace_all(Sys.time(), c(" |:"), "-")
  #   file_name <- paste0("contacts/",input$contact_type, "-", time_stamp, ".csv")
  #   write.csv2(contact_form, file = file_name)
  #   removeModal()
  #   showNotification(sus_translate("Sent and received. Thank you!"), 
  #                    duration = 3)
  # })
  
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
  
  
  ## Heartbeat function to keep app alive --------------------------------------
  
  timeout_start <- eventReactive(reactiveValuesToList(input), Sys.time())
  
  observe({
    rerun <- timeout_start() + 7200 > Sys.time()
    if (rerun) invalidateLater(10000)
  })
  
})
