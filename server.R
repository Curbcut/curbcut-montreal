##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {

  home_server("home", r = r)

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
  
  ## Bookmark ------------------------------------------------------------------
  
  curbcut::use_bookmark(r = r)
  
  
  ## Modules -------------------------------------------------------------------
  
  curbcut::trigger_pages_server(shiny::reactive(input$cc_page), r = r,
                                r_folder_envir = r_folder_envir)

  ## Advanced options ----------------------------------------------------------

  curbcut::settings_advanced(r = r, input = input)


  ## Heartbeat function to keep app alive --------------------------------------
  
  curbcut::heartbeat(input)
  
  observeEvent(input$test, {
    print(hola)
  })
  
})
