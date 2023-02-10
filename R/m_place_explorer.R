### PLACE EXPLORER MODULE ######################################################

# UI ----------------------------------------------------------------------

place_explorer_UI <- function(id) {
  id_map <- paste0(id, "-map")
  
  tagList(
    # Sidebar
    sidebar_UI(
      NS(id, id),
      susSidebarWidgets(
        # Search box
        strong(curbcut::cc_t(translation = translation, 
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
        sliderTextInput(inputId = NS(id, "slider"),
                        label = curbcut::cc_t(translation = translation, 
                                              "Choose scale:"),
                        choices = c("Borough/City", "Census tract", 
                                    "Dissemination area"),
                        selected = "Dissemination area",
                        hide_min_max = TRUE, 
                        force_edges = TRUE),
        # Back button
        actionLink(NS(id, "back"), curbcut::cc_t(translation = translation, 
                                                 "Back to the map"))
      )
    ),
    
    # Map
    div(class = "mapdeck_div", rdeckOutput(NS(id, id_map), height = "100%")),
    
    # Main panel
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
    sidebar_server(id = "place_explorer", r = r, x = "place_explorer")
    
    # df/data reactives
    df <- reactive(get_zoom_code(input$slider))
    data <- reactive(get(paste(r$region(), df(), sep = "_")))
    
    # Map ---------------------------------------------------------------------

    # Main
    output[[id_map]] <- renderRdeck(
      rdeck(map_style = map_style_building, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom)) |> 
        add_mvt_layer(
          id = "place_explorer",
          name = "place_explorer",
          data = mvt_url(paste0("sus-mcgill.", tileset_prefix, "_", 
                                r$region(), "_", df())),
          pickable = TRUE,
          auto_highlight = TRUE,
          highlight_color = "#AAB6CF80",
          get_fill_color = "#AAB6CF20",
          get_line_color = "#FFFFFF00")
    )
    
    # Map click
    observeEvent(get_clicked_object(id_map), {
      selection <- get_clicked_object(id_map)$ID
      if (!is.na(r[[id]]$select_id()) &&
          selection == r[[id]]$select_id()) {
        r[[id]]$select_id(NA)
      } else {
        r[[id]]$select_id(selection)
      }
    })
    
    
    # Select ID behavior ------------------------------------------------------
    
    # Hide main panel when "Go back to map" button is clicked
    observeEvent(input$back, r[[id]]$select_id(NA))
    observeEvent(r[[id]]$select_id(), {
      toggle("back", condition = !is.na(r[[id]]$select_id()))
      toggle("main_panel", condition = !is.na(r[[id]]$select_id()))
    })
    
    
    # Main panel --------------------------------------------------------------
    
    output$main_panel <- renderUI({
      
      if (!is.na(r[[id]]$select_id())) {
        
        # Add the head to the place explorer file
        pe_file <- paste0("www/place_explorer/",
                          r$region(), "_", df(), "_", r[[id]]$select_id(),
                          "_", r$lang(), ".html")

        tmpfile <- tempfile(pattern = "placeex_tmp", 
                            tmpdir = temp_folder, 
                            fileext = ".html")

        head <- normalizePath("www/place_explorer/header.html")
        head <- gsub("/", "\\\\", head)
        pef <- normalizePath(pe_file)
        pef <- gsub("/", "\\\\", pef)
        
        fun <- if (Sys.info()[["sysname"]] == "Windows") "type" else "cat"
        shell(glue::glue("{fun} {head} {pef} > {tmpfile}"))
        
        print(tmpfile)
        
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
    
  })
}
