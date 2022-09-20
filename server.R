##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {

  ## Page title change, depending on page visited ------------------------------

  observe(title_page_update(r = r, session = session, 
                            sus_page = input$sus_page))
  
  
  ## If on mobile, warning! ----------------------------------------------------
  
  observe(mobile_warning(r = r, session = session))
  
  
  ## Reactive variables --------------------------------------------------------
  
  r <- reactiveValues(
    sus_bookmark = reactiveValues(active = FALSE),
    sus_link = reactiveValues(),
    news = reactiveValues(select_id = reactiveVal(NA)),
    lang = reactiveVal("fr"),
    active_tab = "home",
    geo = reactiveVal("CMA"),
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
    natural_inf = reactiveValues(zoom = reactiveVal(9.5)),
    vulnerable_pop = reactiveValues(select_id = reactiveVal(NA), 
                                    df = reactiveVal("centraide"),
                                    zoom = reactiveVal(get_zoom(map_zoom))),
    tenure = reactiveValues(select_id = reactiveVal(NA), 
                            df = reactiveVal("borough"),
                            zoom = reactiveVal(get_zoom(map_zoom)),
                            prev_norm = reactiveVal(FALSE)),
    dw_types = reactiveValues(select_id = reactiveVal(NA), 
                              df = reactiveVal("borough"),
                              zoom = reactiveVal(get_zoom(map_zoom)),
                              prev_norm = reactiveVal(FALSE)),
    demographics = reactiveValues(select_id = reactiveVal(NA), 
                              df = reactiveVal("borough"),
                              zoom = reactiveVal(get_zoom(map_zoom))),
    afford = reactiveValues(select_id = reactiveVal(NA), 
                            df = reactiveVal("borough"),
                            zoom = reactiveVal(get_zoom(map_zoom)),
                            prev_norm = reactiveVal(FALSE))
  )
  

  ## Home page -----------------------------------------------------------------
  
  observe(updateNavbarPage(session, "sus_page", "home")) |> 
    bindEvent(input$title)
  
  
  ## First visit banner --------------------------------------------------------

  # Reset after 14 days of last time the banner was shown

  observeEvent(input$cookies$htu_banner, {
    
    cookie_last <- input$cookies$htu_banner

    if (is.null(cookie_last) || 
        # Show back after 2 weeks
        (!is.null(cookie_last) && Sys.time() > (as.POSIXct(cookie_last) + 1209600))) {
      
      insertUI(selector = ".navbar-shadow", where = "beforeBegin", ui = HTML(
        paste0("<div id = 'htu_footer' class='fixed_footer'>",
               "<p style = 'margin-bottom:0px; color:white; display:inline;'>",
               "Première fois sur Sus? Visitez la page ",
               paste0("<a id='go_to_htu_fr' href='#' style = 'color:white;'",
                      "class='action-button shiny-bound-input'>",
                      "<b>Mode d'emploi</b></a> "),
               "et inscrivez-vous à notre ",
               paste0("<a id='subscribe_fr' href='#' style = 'color:white;'",
                      "class='action-button shiny-bound-input'>",
                      "<b>", "Infolettre", "</b></a> !"),
               "&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;",
               "First time on Sus? Visit the ",
               paste0("<a id='go_to_htu_en' href='#' style = 'color:white;'",
                      "class='action-button shiny-bound-input'>",
                      "<b>How to use page</b></a> "),
               "and subscribe to our ",
               paste0("<a id='subscribe_en' href='#' style = 'color:white;'",
                      "class='action-button shiny-bound-input'>",
                      "<b>", "Newsletter", "</b></a> !"),
               "</p><a id='go_to_htu_x' href='#' ",
               "style = 'float:right;display:inline;color",
               ":#FBFBFB;' class='action-button shiny-bound-input'>X</a>",
               "</div>")))
    }
    
    # After ANY visit, restart the timer
    htu_banner <- list(name = "htu_banner",
                       value = Sys.time())
    session$sendCustomMessage("cookie-set", htu_banner)
    
  }, once = TRUE, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Remove the banner, and log in the cookie
  observeEvent(input$go_to_htu_en, {
    removeUI("#htu_footer")
    updateTabsetPanel(session, "sus_page", selected = "how_to_use")
  }, ignoreInit = TRUE)
  
  observeEvent(input$go_to_htu_fr, {
    removeUI("#htu_footer")
    updateTabsetPanel(session, "sus_page", selected = "how_to_use")
  }, ignoreInit = TRUE)
  
  observeEvent(input$go_to_htu_x, {
    removeUI("#htu_footer")
  }, ignoreInit = TRUE)
  
  onclick("subscribe_fr", {
    showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
                          easyClose = TRUE))
  })
  
  onclick("subscribe_en", {
    showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
                          easyClose = TRUE))
  })
  
  

  ## Newsletter modal ----------------------------------------------------------

  # observeEvent(input$cookies$signupform, {
  # 
  #   cookie_last <- input$cookies$signupform
  # 
  #   # 28 days after last visit, popup the newsletter subscription
  #   if (is.null(cookie_last) ||
  #       (!is.null(cookie_last) && Sys.time() > (as.POSIXct(cookie_last) + 2419200))) {
  #     shinyjs::delay(5000, showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
  #                                                easyClose = TRUE)))
  #   }
  # 
  #   # After ANY visit, restart the timer
  #   signupform <- list(name = "signupform",
  #                      value = Sys.time())
  #   session$sendCustomMessage("cookie-set", signupform)
  # 
  # }, once = TRUE, ignoreNULL = FALSE, ignoreInit = TRUE)

  onclick("subscribe", {
    showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
                          easyClose = TRUE))
  })
  
  
  ## Language button -----------------------------------------------------------
  
  # If the language cookie is "english"
  observeEvent(input$cookies$lang, {
    if (!is.null(input$cookies$lang) && input$cookies$lang == "en") {
      # Set the reactive to english
      r$lang("en")
      # Tell to the JS that all the UI must be in english
      js$setLanguage("en")
    }
  }, once = TRUE)
  
  # A click on the button switches the active language
  observeEvent(input$language_button, {
    r$lang(if (r$lang() == "en") "fr" else "en")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Update the COOKIE
  observeEvent(r$lang(), {
    if (r$lang() == "en") {
      # Tell to the JS that all the UI must be in english
      js$setLanguage("en")
      # Update the label of the language button
      updateActionLink(inputId = "language_button", 
                       label = languageButtonLabel("Français"))
      # Set the cookie to english
      lang_cookie <- list(name = "lang", value = "en")
      session$sendCustomMessage("cookie-set", lang_cookie)
    } else {
      js$setLanguage("fr")
      updateActionLink(inputId = "language_button", 
                       label = languageButtonLabel("English"))
      lang_cookie <- list(name = "lang", value = "fr")
      session$sendCustomMessage("cookie-set", lang_cookie)
    }
  }, ignoreInit = TRUE)
  
  
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
        if (!is.null(tab)) {
          updateTabsetPanel(session, "sus_page", selected = query[["tb"]])
          r$sus_bookmark$id <- tab} else r$sus_bookmark$id <- "home"
      })
      # Update language with query.
      try({
        lang <- query[["lng"]]
        if (!is.null(lang) && lang == "en")
          r$lang <- reactiveVal("en")
      })
      # Update geometries with query.
      try({
        geo <- query[["geo"]]
        if (!is.null(geo)) r$geo(geo)
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
        s_id <- query[["s_id"]]
        if (!is.null(s_id)) {
          if (query[["s_id"]] %in% c("", "NA")) s_id <- NA
          r[[query[["tb"]]]]$select_id <- reactiveVal(s_id)}
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
      r$sus_bookmark$active <- FALSE
    }
  }, priority = -1, once = TRUE)
  
  
  ## Modules -------------------------------------------------------------------
  
  export_data <- list()
  
  active_mod_server <- function(active_tab = input$sus_page) {
    mod_function <- 
      paste0(active_tab, "_server('", active_tab, "', r = r)")

    return(eval(parse(text = mod_function)))
  }

  observeEvent(input$sus_page, {
    bookmark_server(input$sus_page, r = r)

    # Trigger the module server function only if it hasn't been opened already
    if (!input$sus_page %in% r$previous_tabs()) active_mod_server()
    
    updateQueryString("?")
  }, ignoreInit = FALSE)


  ## Advanced options ----------------------------------------------------------

  onclick("advanced_options", {
    showModal(modalDialog(
      radioButtons("geo_change",
                   label = sus_translate(r = r, "Change default geometry"),
                   inline = TRUE,
                   selected = r$geo(),
                   choiceNames = c("CMA", "Centraide"),
                   choiceValues = c("CMA", "centraide")),
      title = sus_translate(r = r, "Advanced options")))
  })
  
  # Change the default geometry and save the cookie
  observeEvent(input$geo_change, {
    r$geo(input$geo_change)
    session$sendCustomMessage("cookie-set", list(name = "default_geo", 
                                                 value = input$geo_change))
  })
  
  # If the geo cookie is already in
  observeEvent(input$cookies$default_geo, {
    if (!is.null(input$cookies$default_geo)) {
      r$geo(input$cookies$default_geo)
    }
  }, once = TRUE)
  
  
  ## Data download -------------------------------------------------------------
  
  data_modal <- reactive(
    data_export_modal(r = r, export_data = r[[input$sus_page]]$export_data()))
  
  onclick("download_data", {
    if (!input$sus_page %in% modules$id || 
        isFALSE(modules$metadata[modules$id == input$sus_page]))
      return(showNotification(
        sus_translate(r = r, "No data/metadata for this location."),
        duration = 3))
    
    showModal(data_modal()$modal)
  })
  
  output$download_csv <-
    downloadHandler(
      filename = paste0(r[[input$sus_page]]$export_data()$id, "_data.csv"),
      content = function(file) {
        data <- data_modal()$data
        write.csv(data, file, row.names = FALSE)
      }, contentType = "text/csv")
  
  output$download_shp <-
    downloadHandler(
      filename = paste0(r[[input$sus_page]]$export_data()$id, "_shp.zip"),
      content = function(file) {
        withProgress(message = sus_translate(r = r, "Exporting Data"), {
          
          incProgress(0.4)
          
          # Prepare data by attaching geometries
          geo <- qread(paste0("data/geometry_export/", 
                              r[[input$sus_page]]$export_data()$data_origin, ".qs"))
          data <- merge(data_modal()$data, geo, by = "ID")
          rm(geo)
          
          incProgress(0.3)
          
          tmp.path <- dirname(file)
          name.base <- file.path(tmp.path,
                                 paste0(r[[input$sus_page]]$export_data()$id, "_data"))
          name.glob <- paste0(name.base, ".*")
          name.shp  <- paste0(name.base, ".shp")
          name.zip  <- paste0(name.base, ".zip")
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
          sf::st_write(data, dsn = name.shp,
                       driver = "ESRI Shapefile", quiet = TRUE)
          
          zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
          req(file.copy(name.zip, file))
          
          incProgress(0.3)
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        })
      })
  
  
  ## Heartbeat function to keep app alive --------------------------------------
  
  timeout_start <- eventReactive(reactiveValuesToList(input), Sys.time())
  
  observe({
    rerun <- timeout_start() + 7200 > Sys.time()
    if (rerun) invalidateLater(10000)
  })
  
})
