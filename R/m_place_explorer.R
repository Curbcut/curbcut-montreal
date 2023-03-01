### PLACE EXPLORER MODULE ######################################################

pe_scales <- stats::setNames(c("CSD", "CT", "DA"), c("CSD", "CT", "DA"))
pe_zoom_levels <- 
  scales_dictionary$slider_title[scales_dictionary$scale %in% pe_scales]
pe_zoom_levels <- stats::setNames(pe_zoom_levels, pe_zoom_levels)


# UI ----------------------------------------------------------------------

place_explorer_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    # Sidebar
    sidebar_UI(
      NS(id, id),
      shiny::div(class = "sus-sidebar-widgets",
        # Search box
        strong(curbcut::cc_t(
                             "Enter postal code or click on the map")),
        HTML(paste0('
                   <div class="shiny-split-layout">
                     <div style="width: 80%;">',
                   textInput(inputId = NS(id, "address_searched"), 
                             label = NULL, placeholder = "H3A 2T5"),
                   '</div>
                     <div style="width: 20%;">',
                   actionButton(inputId = NS(id, "search_button"),
                                label = icon("search", verify_fa = FALSE),
                                style = "margin-top: var(--padding-v-md);"),
                   '</div>
                     </div>')),
        hr(),
        # Scale slider
        curbcut::slider_text_UI(id = NS(id, "slider"),
                                label = "Choose scale:",
                                choices = pe_zoom_levels,
                                selected = pe_zoom_levels[length(pe_zoom_levels)]),
        
        # Back button
        actionLink(NS(id, "back"), curbcut::cc_t(
                                                 "Back to the map"))
      )
    ),
    
    # Map
    div(class = "map_div", rdeckOutput(NS(id, id_map), height = "100%")),
    
    # Main panel
    hidden(htmlOutput(
      NS(id, "loader"))),
    hidden(htmlOutput(
      NS(id, "main_panel")))

  )
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    id_map <- paste0(id, "-map")
    
    
    # Misc --------------------------------------------------------------------
    
    # Sidebar
    sidebar_server(id = "place_explorer", r = r)
    
    # Get the output of the slider
    slider <- curbcut::slider_text_server(id = "slider",
                                          r = r,
                                          choices = shiny::reactive(pe_zoom_levels))
    
    # df/data reactives
    observeEvent(slider(), {
      r[[id]]$df(zoom_get_code(slider()))
    })
    data <- reactive(get(paste(r$region(), r[[id]]$df(), sep = "_")))
    
    
    # Translate slider --------------------------------------------------------
    
    # 
    # observe(updateSliderTextInput(
    #   session = session, "zoom_slider",
    #   choices = zoom_get_label(pe_scales, lang = r$lang())))
    # 
    # # Tweak for bookmark
    # observeEvent(parseQueryString(session$clientData$url_search)$tb, {
    #   if (parseQueryString(session$clientData$url_search)$tb == "place_explorer") {
    #     df <- parseQueryString(session$clientData$url_search)$df
    #     df <- stats::setNames(df, df)
    #     updateSliderTextInput(
    #       session = session, "zoom_slider",
    #       choices = zoom_get_label(pe_scales, lang = r$lang()),
    #       selected = zoom_get_label(df, lang = r$lang()))
    #   }
    # }, once = TRUE)
    
    
    # Postal code search ------------------------------------------------------
    
    observeEvent(input$search_button, {
      postal_c <-
        tolower(input$address_searched) |>
        stringr::str_extract_all("\\w|\\d", simplify = TRUE) |>
        paste(collapse = "")
      print(postal_c)
      
      DA_id <- postal_codes$DA_ID[postal_codes$postal_code == postal_c]
      
      if (length(DA_id) == 0) {
        showNotification(
          cc_t(lang = r$lang(),
               paste0("No postal code found for `", 
                      input$address_searched, "`")),
          type = "error")
      } else {
        DA_table <- get0(paste0(r$region(), "_DA"))
        if (is.null(DA_table)) return(NULL)
        right_id <- DA_table[[paste0(r[[id]]$df(), "_ID")]][DA_table$ID == DA_id]
        r[[id]]$select_id(right_id)
      }
    })
    
    
    # Map ---------------------------------------------------------------------

    # Main
    output[[id_map]] <- renderRdeck(
      rdeck(map_style = map_style_building, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom)) |> 
        add_mvt_layer(
          id = "place_explorer",
          name = "place_explorer",
          data = mvt_url(paste0("sus-mcgill.", tileset_prefix, "_", 
                                r$region(), "_", r[[id]]$df())),
          pickable = TRUE,
          auto_highlight = TRUE,
          highlight_color = "#AAB6CF80",
          get_fill_color = "#AAB6CF20",
          get_line_color = "#FFFFFF00")
    )
    
    # Map click
    curbcut::update_select_id(id = id, r = r)
    
    
    # Select ID behavior ------------------------------------------------------
    
    # Hide main panel when "Go back to map" button is clicked
    observeEvent(input$back, r[[id]]$select_id(NA))
    observeEvent(r[[id]]$select_id(), {
      toggle("back", condition = !is.na(r[[id]]$select_id()))
      toggle("loader", condition = !is.na(r[[id]]$select_id()))
      toggle("main_panel", condition = !is.na(r[[id]]$select_id()))
      toggle("zoom_slider", condition = is.na(r[[id]]$select_id()))
    })
    
    
    # Main panel --------------------------------------------------------------
    
    output$loader <- renderUI({
      if (!is.na(r[[id]]$select_id())) {
        div(
          class = "main_panel_popup loader-page", 
          style = "height:calc(100vh - 100px);overflow:hidden;",
          tags$iframe(
            style = "width:100%;height:100%;",
            title = "place_ex",
            srcdoc = paste0("<!DOCTYPE html><html><head><title></title><style",
                            ">body {background-color: white;}</style></head><",
                            "body><body></html>"),
            frameborder = 0)
        )
      }
    })
    
    output$main_panel <- renderUI({
      
      if (!is.na(r[[id]]$select_id())) {
        

        # Add the head to the place explorer file
        pe_file <- paste0("www/place_explorer/",
                          r$region(), "_", r[[id]]$df(), "_", r[[id]]$select_id(),
                          "_", r$lang(), ".html")

        tmpfile <- tempfile(pattern = "placeex_tmp",
                            tmpdir = temp_folder,
                            fileext = ".html")

        head <- normalizePath("www/place_explorer/header.html")
        head <- gsub("/", "\\\\", head)
        pef <- normalizePath(pe_file)
        pef <- gsub("/", "\\\\", pef)

        fun <- if (Sys.info()[["sysname"]] == "Windows") "type" else "cat"
        shell(sprintf("%s %s %s > %s", fun, head, pef, tmpfile))

        tmpfile <- stringr::str_extract(tmpfile, "placeex_tmp.*$")

        # Show the file
        div(class = "main_panel_popup",
            style = "height:100%;overflow:hidden;",
            tags$iframe(style = "width:100%;height:100%;",
                        title = "place_ex",
                        src = file.path("temp_folder_shortcut", tmpfile),
                        frameborder = 0)
        )
        
      }
    })
    
    
    # Bookmarking -------------------------------------------------------------
    
    curbcut::bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id
    )
    
  })
}
