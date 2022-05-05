##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  

  # Page title change, depending on page visited -------------------------------

  observe(title_page_update(r = r, session = session, sus_page = input$sus_page))
  
  # If on Mobile, warning! -----------------------------------------------------
  
  observe(mobile_warning(r = r, session = session))
  
  
  # Reactive variables ---------------------------------------------------------
  
  r <- reactiveValues(sus_bookmark = reactiveValues(active = FALSE),
                      sus_link = reactiveValues(),
                      lang = "fr",
                      active_tab = "home")
  
  # Home page ------------------------------------------------------------------
  
  observe(updateNavbarPage(session, "sus_page", "home")) |> 
    bindEvent(input$title)
  
  
  # First visit banner ---------------------------------------------------------
  
  # Reset after 14 days of last time the banner was shown
  observeEvent(input$cookies$time_last_htu_banner, {
    if (is.null(input$cookies$time_last_htu_banner) ||
        (!is.null(input$cookies$time_last_htu_banner) &&
         Sys.time() > (as.POSIXct(input$cookies$time_last_htu_banner) + 1209600))) {
      
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
      click("language_button")
    # COOKIE, runs only once at launch !
  }, once = TRUE)
  
  observeEvent(input$language_button, {
    r$lang <- if (input$language_button[1] %% 2 != 0) "en" else "fr"
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
      update_module(mod_ns = r$sus_link$mod_ns, 
                    session = session, 
                    zoom = r$sus_link$zoom, 
                    location = r$sus_link$location, 
                    map_id = r$sus_link$map_id,
                    df = r$sus_link$df, 
                    zoom_auto = r$sus_link$zoom_auto, 
                    var_left = r$sus_link$var_left,
                    var_right = r$sus_link$var_right, 
                    more_args = r$sus_link$more_args)
    })
  }, ignoreInit = TRUE)
  
  
  ## Parse URL -----------------------------------------------------------------
  
  observe({
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
          r$sus_bookmark$zoom <- as.numeric(query[["zm"]])}
        
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
      # Retrieve select_id
      try({
        if (!is.null(query[["s_id"]]))
          r$sus_bookmark$select_id <- query[["s_id"]]
      })
      # Retrieve if df is manual
      try({
        if (!is.null(query[["zm_a"]])) {
          r$sus_bookmark$zoom_auto <- as.logical(query[["zm_a"]])
          r$sus_bookmark$df <- query[["df"]]}
      })
      try({
        if (!is.null(query[["more"]]))
          r$sus_bookmark$more_args <- query[["more"]]
      })
    }
  })
  
  
  ## Modules -------------------------------------------------------------------
  
  active_mod_server <- function(active_tab = input$sus_page) {
    mod_function <- 
      paste0(active_tab, "_server('", active_tab, "', r = r)")

    # Run the function but also catch its output for data exportation
    assign("export_data", eval(parse(text = mod_function)), 
           pos = 1)
  }

  observeEvent(input$sus_page, {
    bookmark_server("reset", r = r)
    if (!is.null(r$sus_bookmark$active) && isTRUE(r$sus_bookmark$active)) 
      active_mod_server()
    if (!input$sus_page %in% r$previous_tabs()) active_mod_server()
    updateQueryString("?")
  }, ignoreInit = FALSE)
  
  
  ## Data download -------------------------------------------------------------
  
  data_modal <- reactive(data_export_modal(r = r, export_data = export_data()))
  
  onclick("download_data", {
    if (!input$sus_page %in% modules$id || 
        isFALSE(modules$metadata[modules$id == input$sus_page]))
      return(showNotification(
        sus_translate(r = r, "No data/metadata for this location."),
        duration = 3))
    
    showModal(data_modal()$modal)
  })
  
  # observeEvent(export_data(), {assign("export_data1", export_data(), pos = 1)},
  #              ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$download_csv <-
    downloadHandler(
      filename = paste0(export_data()$id, "_data.csv"),
      content = function(file) {
        data <- data_modal()$data
        write.csv(data, file, row.names = FALSE)
      }, contentType = "text/csv")
  
  output$download_shp <-
    downloadHandler(
      filename = paste0(export_data()$id, "_shp.zip"),
      content = function(file) {
        withProgress(message = "Exporting Data", {
          
          incProgress(0.4)
          
          # Prepare data by attaching geometries
          geo <- qread(paste0("data/geometry_export/", 
                              export_data()$data_origin, ".qs"))
          data <- merge(data_modal()$modal, geo, by = "ID")
          rm(geo)
          
          incProgress(0.3)
          
          tmp.path <- dirname(file)
          name.base <- file.path(tmp.path,
                                 paste0(export_data()$id, "_data"))
          name.glob <- paste0(name.base, ".*")
          name.shp  <- paste0(name.base, ".shp")
          name.zip  <- paste0(name.base, ".zip")
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
          invisible(sf::st_write(data, dsn = name.shp,
                       driver = "ESRI Shapefile", quiet = TRUE))
          
          zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
          req(file.copy(name.zip, file))
          
          incProgress(0.3)
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
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
  
  # output$create_report <-
  #   downloadHandler(
  #     filename = "report.html",
  #     content = function(file) {
  #       shiny::withProgress(
  #         message = sus_translate(paste0("Generating report on ",
  #                                        active_mod()$module_short_title)),
  #         {
  #           shiny::incProgress(0.35)
  #           tempReport <- file.path(tempdir(), "report.Rmd")
  #           file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
  #           params <- list(
  #             module_short_title = active_mod()$module_short_title,
  #             module = active_mod()$module_id,
  #             map_title = title_text$text[
  #               title_text$tab == active_mod()$module_id & 
  #                 title_text$type == "title"],
  #             time = active_mod()$time,
  #             data = active_mod()$data,
  #             token = active_mod()$token,
  #             map_zoom = active_mod()$map_zoom,
  #             map_loc = active_mod()$map_loc,
  #             df = active_mod()$df,
  #             explore_content = active_mod()$explore_content,
  #             poly_selected = active_mod()$poly_selected,
  #             legend_graph = active_mod()$legend_graph)
  #           shiny::incProgress(0.35)
  #           rmarkdown::render(tempReport, output_file = file,
  #                             params = params,
  #                             envir = new.env(parent = globalenv()))
  #           shiny::incProgress(0.3)
  #         })
  #     }
  #   )
  
  ## Heartbeat function to keep app alive --------------------------------------
  
  timeout_start <- eventReactive(reactiveValuesToList(input), Sys.time())
  
  observe({
    rerun <- timeout_start() + 1800 > Sys.time()
    if (rerun) invalidateLater(10000)
  })
  
})
