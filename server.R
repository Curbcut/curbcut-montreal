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
