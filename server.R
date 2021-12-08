##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  observeEvent(input$title, {
    updateNavbarPage(session, "sus_page", "home")
  })
  
  # Language button ---------------------------------------------------------
  
  sus_rv$lang <- 
    eventReactive(input$language_button, {
      if (input$language_button[1] %% 2 != 0) "en" else "fr"
    }, ignoreNULL = FALSE)
  
  observeEvent(input$language_button,{
    if (input$language_button[1] %% 2 != 0) {
      update_lang(session, "en")
      updateActionLink(inputId = "language_button", label = "Français")
    } else {
      update_lang(session, "fr")
      updateActionLink(inputId = "language_button", label = "English")
      
    }
  })
  
  
  # Modules -----------------------------------------------------------------
  
  home_server("home")
  
  active_mod_server <- function(active_tab = input$sus_page) {
    if (active_tab == "access") {
      access_server("access")
    } else if (active_tab == "alley") {
      alley_server("alley")    
    } else if (active_tab == "canale") {
      active_mod <<- canale_server("canale")
    } else if (active_tab == "climate_risk") {
      climate_risk_server("climate_risk")
    } else if (active_tab == "covid") {
      covid_server("covid")
    } else if (active_tab == "crash") {
      crash_server("crash")
    } else if (active_tab == "gentrification") {
      active_mod <<- gentrification_server("gentrification")
    } else if (active_tab == "housing") {
      housing_server("housing")
    } else if (active_tab == "mcp") {
      mcp_server("mcp")
    } else if (active_tab == "stories") {
      stories_server("stories")
    } else if (active_tab == "about") {
      why_dash_server("why_dash")
    }
  }
  
  observeEvent(input$sus_page, {
    active_mod_server()
  }, ignoreInit = TRUE)
  
  onRestore(function(state) {
    active_mod_server()
  })
  
  # Data download -----------------------------------------------------------
  
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
    downloadHandler(filename = paste0(active_mod()$module_id, "_data.csv"),
                    content = function(file) {
                      data <- st_drop_geometry(active_mod()$data)
                      write.csv2(data, file)
                    },
                    contentType = "text/csv")
  
  output$download_shp <-
    downloadHandler(filename = paste0(active_mod()$module_id, "_shp.zip"),
                    content = function(file) {
                      withProgress(message = "Exporting Data", {
                        
                        incProgress(0.5)
                        tmp.path <- dirname(file)
                        
                        name.base <- file.path(tmp.path, paste0(active_mod()$module_id, "_data"))
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
  
  
  # Contact form ------------------------------------------------------------
  
  contactModal <- function() {
    modalDialog(
      selectInput("contact_type", "Reason of contact",
                  choices = c("Contact" = "CONTACT",
                              "Report a bug" = "BUG",
                              "Feedback" = "FEEDBACK",
                              "Other" = "OTHER"), width = "75%"),
      textInput("contact_from_name", "Your name and organization", width = "75%"),
      textInput("contact_from", "Your email adress", "@", width = "75%"),
      textInput("contact_subject", "Subject", width = "75%"),
      textAreaInput("contact_body", "Content", width = "75%", height = "300px"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("send_feedback", "Send")
      ),
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
    showNotification(sus_translate("Sent and received. Thank you!"), duration = 1.5)
  })
  
  
  # Generating report -------------------------------------------------------
  
  output$create_report <-
    downloadHandler(filename = "report.html",
                    content = function(file) {
                      shiny::withProgress(
                        message = sus_translate(paste0("Generating report on ",
                                                       active_mod()$module_short_title)),
                        {
                          shiny::incProgress(0.35)
                          tempReport <- file.path(tempdir(), "report.Rmd")
                          file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
                          params <- list(module_short_title = active_mod()$module_short_title,
                                         module = active_mod()$module_id,
                                         map_title = (filter(filter(title_text,
                                                                    tab == active_mod()$module_id),
                                                             type == "title"))$text,
                                         time = active_mod()$time,
                                         data = active_mod()$data,
                                         token = active_mod()$token,
                                         map_zoom = active_mod()$map_zoom,
                                         map_location = active_mod()$map_location,
                                         zoom = active_mod()$zoom,
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
  
  # Waiter ------------------------------------------------------------------
  
  waiter_hide()
  
})
