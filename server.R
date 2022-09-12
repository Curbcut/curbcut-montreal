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
  

  ## Change geometries on button clicked or on cookie --------------------------
  
  observeEvent(input$geo_CMA, {
    r$geo("CMA")
    session$sendCustomMessage("cookie-set", list(name = "default_geo", 
                                                 value = "CMA"))
  })
  
  observeEvent(input$geo_centraide, {
    r$geo("centraide")
    session$sendCustomMessage("cookie-set", list(name = "default_geo", 
                                                 value = "centraide"))
  })
  
  # If the language cookie is "english"
  observeEvent(input$cookies$default_geo, {
    if (!is.null(input$cookies$default_geo)) {
      r$geo(input$cookies$default_geo)
    }
  }, once = TRUE)
  
  
  ## Heartbeat function to keep app alive --------------------------------------
  
  timeout_start <- eventReactive(reactiveValuesToList(input), Sys.time())
  
  observe({
    rerun <- timeout_start() + 7200 > Sys.time()
    if (rerun) invalidateLater(10000)
  })
  
})
