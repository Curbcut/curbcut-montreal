##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {

  ## Page title change, depending on page visited ------------------------------

  observe(title_page_update(r = r, session = session, 
                            sus_page = input$sus_page))
  
  
  ## If on mobile, warning! ----------------------------------------------------
  
  observe(mobile_warning(r = r, session = session))
  
  
  ## If crash, personalized error ----------------------------------------------
  
  observe({
      sever(html = severe_html(lang = r$lang(),
                               module_id = input$sus_page,
                               geo = r$geo()),
            bg_color = "rgba(0,0,0,.5)", box = TRUE)
  })

  
  ## Reactive variables --------------------------------------------------------
  
  r <- reactiveValues(
    sus_bookmark = reactiveValues(active = FALSE),
    sus_link = reactiveValues(),
    news = reactiveValues(select_id = reactiveVal(NA)),
    lang = reactiveVal("fr"),
    active_tab = "home",
    geo = reactiveVal(default_region),
    default_select_id = reactiveVal(NULL),
    stories = reactiveValues(select_id = reactiveVal(NA)))
  
  
  for (i in modules$id) {
    df <- modules$regions[modules$id == i]
    df <- unlist(df)[[1]]
    if (is.null(df)) {
      r[[i]] <- reactiveValues(
        select_id = reactiveVal(NA),
        zoom = reactiveVal(get_zoom(map_zoom)),
        prev_norm = reactiveVal(FALSE))
    } else {
      df <- if (is.null(df)) default_region else df
      df <- paste(df, names(get(paste0("map_zoom_levels_", df)))[[1]], sep = "_")
      r[[i]] <- reactiveValues(
        select_id = reactiveVal(NA),
        df = reactiveVal(df),
        zoom = reactiveVal(get_zoom(map_zoom)),
        prev_norm = reactiveVal(FALSE))
    }
  }

  
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
               "Première fois sur Curbcut? Visitez la page ",
               paste0("<a id='go_to_htu_fr' href='#' style = 'color:white;'",
                      "class='action-button shiny-bound-input'>",
                      "<b>Mode d'emploi</b></a> "),
               "et inscrivez-vous à notre ",
               paste0("<a id='subscribe_fr' href='#' style = 'color:white;'",
                      "class='action-button shiny-bound-input'>",
                      "<b>", "Infolettre", "</b></a> !"),
               "&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;",
               "First time on Curbcut? Visit the ",
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
  
  

  ## Newsletter ----------------------------------------------------------------

  onclick("subscribe", {
    showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
                          easyClose = TRUE))
  })
  
  onclick("sign_up_from_carousel", {
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
                    zoom = r$sus_link$zoom, 
                    location = r$sus_link$location,
                    zoom_auto = r$sus_link$zoom_auto,
                    var_left = r$sus_link$var_left,
                    var_right = r$sus_link$var_right,
                    select_id = r$sus_link$select_id,
                    df = r$sus_link$df,
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
        if (!is.null(query[["df"]])) {
          r[[query[["tb"]]]]$df <- reactiveVal(query[["df"]])
        }
      })
      # Retrieve select_id
      try({
        s_id <- query[["s_id"]]
        if (!is.null(s_id)) {
          if (query[["s_id"]] %in% c("", "NA")) s_id <- NA
          r[[query[["tb"]]]]$select_id <- reactiveVal(s_id)
          }
      })
    }
  }, once = TRUE)

  # Update from bookmark
  observeEvent(r$sus_bookmark$active, {
    if (isTRUE(r$sus_bookmark$active)) {
      # Delay to make sure the bookmarked module is fully loaded
      delay(500, {
        zz <- if (!is.null(r[[input$sus_page]]$zoom)) 
          r[[input$sus_page]]$zoom() else NULL
        update_module(session = session,
                      r = r,
                      id = r$sus_bookmark$id,
                      map_id = "map",
                      zoom = zz,
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
  
  active_mod_server <- function(active_tab = input$sus_page) {
    do.call(paste0(active_tab, "_server"), list(active_tab, r = r))
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
      # Change 'geo' (region)
      radioButtons("geo_change",
                   label = cc_t(r = r, "Change default geometry"),
                   inline = TRUE,
                   selected = r$geo(),
                   choiceNames = 
                     sapply(regions_dictionary$name[regions_dictionary$pickable],
                            cc_t, r = r) |> unname(),
                   choiceValues = regions_dictionary$geo[regions_dictionary$pickable]),

      hr(),
      
      # Lock in address of zone for select_ids
      strong(cc_t(r = r, "Enter and save a default location (postal code or ",
                  "address)")),
      HTML("<br><i>", cc_t(r = r, "Default location will be saved until ",
                    "manually cleared from advanced options"), "</i>"),
      HTML(paste0('<div class="shiny-split-layout">',
                  '<div style="width: 80%; margin-top: var(--padding-v-md); ',
                  'width:auto;">',
                  textInput(inputId = "lock_address_searched", 
                            label = NULL, 
                            placeholder = default_random_address),
                  '</div><div style="width: 20%">',
                  actionButton(inputId = "lock_search_button",
                               label = icon("check", verify_fa = FALSE),
                               style = "margin-top: var(--padding-v-md);"),
                  '</div></div>',
                  actionButton(inputId = "cancel_lock_location",
                               label = cc_t(r = r, "Clear default location"), 
                               icon = icon("xmark", verify_fa = FALSE),
                               style = "margin-top: var(--padding-v-md);"))),
      title = cc_t(r = r, "Advanced options"),
      footer = modalButton(cc_t(r = r, "Dismiss"))))
    })

  # Change the default geometry and save the cookie
  observeEvent(input$geo_change, {
    r$geo(input$geo_change)
    session$sendCustomMessage("cookie-set", list(name = "default_geo",
                                                 value = input$geo_change))
  })

  # If the geo cookie is already in and it differs from default
  observeEvent(input$cookies$default_geo, {
    if (!is.null(input$cookies$default_geo) &&
        input$cookies$default_geo != default_region) {
      r$geo(input$cookies$default_geo)
    }
  }, once = TRUE)
  
  # Location lock in
  observeEvent(input$lock_search_button, {
    postal_c <-
      input$lock_address_searched |>
      str_to_lower() |>
      str_extract_all("\\w|\\d", simplify = TRUE) |>
      paste(collapse = "")
    pcs <- postal_codes$postal_code == postal_c
    
    out <- 
      if (sum(pcs) > 0 || grepl("[a-z][0-9][a-z][0-9][a-z][0-9]", postal_c)) {
        # Postal code detected, but not in our database
        if (sum(pcs) == 0) {
          showNotification(
            cc_t(r = r, "Postal code `{postal_c}` isn't within an ",
                 "available geography."),
            type = "error")
          return(NULL)
        }
        
        showNotification(
          cc_t(r = r,
               paste0("Postal code `{postal_codes$postal_code[pcs]}` ",
                      "saved as default.")),
          type = "default")
        
        sapply(regions_dictionary$geo, \(x) {
          dat <- get0(paste0(x, "_DA"))
          if (is.null(dat)) return("")
          dat <- dat[dat$ID == postal_codes$DA_ID[pcs], ]
          if (length(dat) == 0) {
            showNotification(
              cc_t(r = r, paste0("No addresses found.")),
              type = "error")
            return(NULL)
          }
          unique(unlist(dat[grepl("ID$", names(dat))]))
        }, simplify = FALSE, USE.NAMES = TRUE)
        
      } else {
        ad <- input$lock_address_searched
        add <- ad
        # Convert to ASCII
        add <- paste0("%", charToRaw(add), collapse = "")
        add <- paste0("http://geogratis.gc.ca/services/geolocation/en/locate?q=",
                      add)
        
        get <- httr::GET(add)
        val <- httr::content(get)
        if (length(val) == 0) {
          showNotification(
            cc_t(r = r, paste0("No addresses found.")),
            type = "error")
          return(NULL)
        }
        val <- val[[1]]
        coords <- val$geometry$coordinates
        
        out <- 
          sapply(regions_dictionary$geo, \(x) {
            scales <- names(get(paste0("map_zoom_levels_", x)))
            if (length(which("DA" == scales)) > 0) {
              scales <- scales[seq_len(which("DA" == scales))]
            }
            last_scales <- scales[length(scales)]
            da_vals <- do.call("dbGetQuery", list(rlang::sym(paste0(x, "_", 
                                                                    last_scales, 
                                                                    "_conn")),
                                                  paste0("SELECT * FROM centroid")))
            distance_sum <- 
              mapply(sum, abs(coords[[1]] - da_vals$lat), abs(coords[[2]] - da_vals$lon))
            # If too far.
            if (min(distance_sum) > 0.1) return(NULL)
            dat_ID <- da_vals$ID[which(distance_sum == min(distance_sum))]
            dat <- get(paste(x, last_scales, sep = "_"))
            dat <- dat[dat$ID == dat_ID, ]
            na.omit(unique(unlist(dat[grepl("ID$", names(dat))])))
          }, simplify = FALSE, USE.NAMES = TRUE)
        
        if (all(sapply(out, is.null))) {
          showNotification(
            cc_t(r = r, "Address `{input$lock_address_searched}` isn't",
                 " within an available geography."),
            type = "error")
          out <- NULL
        } else {
          showNotification(
            cc_t(r = r, "Address `{val$title}` saved as default."),
            type = "default")          
        }
        
        out
      }
    
    r$default_select_id(out)
  })
  
  observeEvent(input$cancel_lock_location, {
    r$default_select_id(NULL)
    
    showNotification(
      cc_t(r = r,
                    paste0("Default location successfully cleared")),
      type = "default")
  })
  
  
  ## Data download -------------------------------------------------------------
  
  data_modal <- reactive(
    data_export_modal(r = r, export_data = r[[input$sus_page]]$export_data()))
  
  onclick("download_data", {
    if (!input$sus_page %in% modules$id || 
        isFALSE(modules$metadata[modules$id == input$sus_page]))
      return(showNotification(
        cc_t(r = r, "No data/metadata for this location."),
        duration = 3))
    
    showModal(data_modal()$modal)
  })
  
  output$download_csv <-
    downloadHandler(
      filename = reactive(paste0(r[[input$sus_page]]$export_data()$id, "_data.csv")),
      content = function(file) {
        data <- data_modal()$data
        write.csv(data, file, row.names = FALSE)
      }, contentType = "text/csv")
  
  output$download_shp <-
    downloadHandler(
      filename = reactive(paste0(r[[input$sus_page]]$export_data()$id, "_shp.zip")),
      content = function(file) {
        withProgress(message = cc_t(r = r, "Exporting Data"), {
          
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
  
  
  # ## Screenshot ----------------------------------------------------------------
  # 
  # onclick("save_image", {
  #   js$takeShot(to_sh_id = "housing-housing-map", output_id = "screenshot_container")
  #   showModal(modalDialog(
  #     div(id = "screenshot_container"),
  #     title = cc_t(r = r, "Save as image"),
  #     size = "l"
  #   ))
  # })
  
  ## Heartbeat function to keep app alive --------------------------------------
  
  timeout_start <- eventReactive(reactiveValuesToList(input), Sys.time())
  
  observe({
    rerun <- timeout_start() + 7200 > Sys.time()
    if (rerun) invalidateLater(10000)
  })
  
})
