##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  ## If crash, personalized error ----------------------------------------------
  
  observe({
      sever(html = severe_html(lang = r$lang(),
                               module_id = input$cc_page,
                               region = r$region()),
            bg_color = "rgba(0,0,0,.5)", box = TRUE)
  })

  
  ## Reactive variables --------------------------------------------------------
  
  r <- r_init(server_session = session,
              lang_init = "fr", 
              prev_norm = shiny::reactiveVal(FALSE))
  
  ## Page title change, depending on page visited ------------------------------
  
  curbcut::title_page_update(r = r, 
                             active_page = shiny::reactive(input$cc_page), 
                             site_name = site_name)
  
  ## If on mobile, warning! ----------------------------------------------------
  
  curbcut::mobile_warning(r = r)

  
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

  # Click here comes from a JS script which appends the Newsletter option
  # in the `About` navbarMenu (www/acount_contact.js).
  observeEvent(input$newsletter_click, {
    showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
                          easyClose = TRUE))
  })
  
  onclick("sign_up_from_carousel", {
    showModal(modalDialog(HTML(readLines("www/sus.signupform.html")),
                          easyClose = TRUE))
  })
  
  ## Language button -----------------------------------------------------------
  
  curbcut::language_server(r = r)
  
  
  ## Bookmark ------------------------------------------------------------------
  
  curbcut::use_bookmark(r = r)
  
  
  ## Modules -------------------------------------------------------------------
  
  curbcut::trigger_pages_server(shiny::reactive(input$cc_page), r = r,
                                r_folder_envir = r_folder_envir)


  ## Advanced options ----------------------------------------------------------

  curbcut::settings_server(r = r)


  ## Heartbeat function to keep app alive --------------------------------------
  
  curbcut::heartbeat(input)
  
})
