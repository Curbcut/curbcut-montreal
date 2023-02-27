##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {

  ## Page title change, depending on page visited ------------------------------
  
  curbcut::title_page_update(r = r, parent_session = session, 
                             active_page = shiny::reactive(input$cc_page), 
                             site_name = site_name)
  
  ## If on mobile, warning! ----------------------------------------------------
  
  curbcut::mobile_warning(r = r, parent_session = session)
  
  
  ## If crash, personalized error ----------------------------------------------
  
  observe({
      sever(html = severe_html(lang = r$lang(),
                               module_id = input$cc_page,
                               region = r$region()),
            bg_color = "rgba(0,0,0,.5)", box = TRUE)
  })

  
  ## Reactive variables --------------------------------------------------------
  
  r <- reactiveValues(
    sus_bookmark = reactiveValues(active = FALSE),
    sus_link = reactiveValues(),
    news = reactiveValues(select_id = reactiveVal(NA)),
    lang = reactiveVal("fr"),
    active_tab = "home",
    region = reactiveVal(default_region),
    default_select_ids = reactiveVal(NULL),
    stories = reactiveValues(select_id = reactiveVal(NA)),
    place_explorer = reactiveValues(select_id = reactiveVal(NA),
                                    df = reactiveVal("DA")))
  
  
  for (i in modules$id) {
    df <- modules$regions[modules$id == i]
    df <- unlist(df)[[1]]
    if (is.null(df)) {
      r[[i]] <- reactiveValues(
        select_id = reactiveVal(NA),
        zoom = reactiveVal(curbcut::zoom_get(map_zoom)),
        coords = reactiveVal(map_loc),
        poi = reactiveVal(NULL),
        prev_norm = reactiveVal(FALSE))
    } else {
      df <- if (is.null(df)) default_region else df
      df <- paste(df, names(get(paste0("map_zoom_levels_", df)))[[1]], sep = "_")
      r[[i]] <- reactiveValues(
        select_id = reactiveVal(NA),
        df = reactiveVal(df),
        zoom = reactiveVal(curbcut::zoom_get(map_zoom)),
        coords = reactiveVal(map_loc),
        poi = reactiveVal(NULL),
        prev_norm = reactiveVal(FALSE))
    }
  }

  
  ## Home page -----------------------------------------------------------------
  
  # When the Curbcut title on the left corner is clicked on
  shiny::observeEvent(input$title, updateNavbarPage(session, "cc_page", "home"))
  
  
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
    updateTabsetPanel(session, "cc_page", selected = "how_to_use")
  }, ignoreInit = TRUE)
  
  observeEvent(input$go_to_htu_fr, {
    removeUI("#htu_footer")
    updateTabsetPanel(session, "cc_page", selected = "how_to_use")
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

  onclick("settings-subscribe", {
    showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
                          easyClose = TRUE))
  })
  
  onclick("sign_up_from_carousel", {
    showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
                          easyClose = TRUE))
  })
  
  ## Language button -----------------------------------------------------------
  
  curbcut::language_server(r = r, parent_session = session)
  
  
  ## Active tab ----------------------------------------------------------------
  
  observeEvent(input$cc_page, r$active_tab <- input$cc_page, 
               ignoreNULL = FALSE)
  
  observeEvent(input$cc_page, {
    r$last_module <- unique(c(r$current_module, r$last_module))
    r$current_module <- c(input$cc_page, r$last_module)})
  
  r$previous_tabs <- reactive({
    req(input$cc_page)
    input$cc_page
    r$last_module})
  
  observeEvent(r$link, {
    # Switch active tab when link is opened
    updateTabsetPanel(session, "cc_page", selected = r$link)
    # Turn off the link
    r$link <- NULL
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  ## Bookmark ------------------------------------------------------------------
  
  curbcut::use_bookmark(r = r, parent_session = session)
  
  
  ## Modules -------------------------------------------------------------------
  
  active_mod_server <- function(active_tab = input$cc_page) {
    do.call(paste0(active_tab, "_server"), list(active_tab, r = r))
  }

  observeEvent(input$cc_page, {

    # Trigger the module server function only if it hasn't been opened already
    if (!input$cc_page %in% r$previous_tabs()) active_mod_server()
    
    updateQueryString("?")
  }, ignoreInit = FALSE)


  ## Advanced options ----------------------------------------------------------

  curbcut::settings_server(r = r, parent_session = session)
  
  ## Data download -------------------------------------------------------------
  
  data_modal <- reactive(
    data_export_modal(r = r, export_data = r[[input$cc_page]]$export_data()))
  
  onclick("settings-download_data", {
    if (!input$cc_page %in% modules$id || 
        isFALSE(modules$metadata[modules$id == input$cc_page]))
      return(showNotification(
        curbcut::cc_t(lang = r$lang(), 
                      "No data/metadata for this location."),
        duration = 3))
    
    showModal(data_modal()$modal)
  })
  
  output$download_csv <-
    downloadHandler(
      filename = reactive(paste0(r[[input$cc_page]]$export_data()$id, "_data.csv")),
      content = function(file) {
        data <- data_modal()$data
        write.csv(data, file, row.names = FALSE)
      }, contentType = "text/csv")
  
  output$download_shp <-
    downloadHandler(
      filename = reactive(paste0(r[[input$cc_page]]$export_data()$id, "_shp.zip")),
      content = function(file) {
        withProgress(message = curbcut::cc_t(lang = r$lang(), 
                                             "Exporting Data"), {
          
          incProgress(0.4)
          
          # Prepare data by attaching geometries
          geo <- qread(paste0("data/geometry_export/", 
                              r[[input$cc_page]]$export_data()$data_origin, ".qs"))
          data <- merge(data_modal()$data, geo, by = "ID")
          rm(geo)
          
          incProgress(0.3)
          
          tmp.path <- dirname(file)
          name.base <- file.path(tmp.path,
                                 paste0(r[[input$cc_page]]$export_data()$id, "_data"))
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
