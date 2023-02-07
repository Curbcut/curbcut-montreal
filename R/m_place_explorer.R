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
                        choices = c("Borough/city", "Census tract", 
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
          data = mvt_url(paste0(mapbox_username, ".", r$region(), "_DA")),
          pickable = TRUE,
          auto_highlight = TRUE,
          highlight_color = "#AAB6CF80",
          get_fill_color = "#AAB6CF20",
          get_line_color = "#FFFFFF00")
    )
    
    # Update main map when the chosen scale changes
    rdeck_proxy(id = id_map) |>
      add_mvt_layer(id = "place_explorer",
                    data = mvt_url(paste0("sus-mcgill.", r$region(), "_", df())),
                    pickable = TRUE,
                    auto_highlight = TRUE,
                    highlight_color = "#AAB6CF80",
                    get_fill_color = "#AAB6CF20",
                    get_line_color = "#FFFFFF00")
    
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
        
        title <- 
          # If the `name` column isn't a character
          if (all(is.na(stringr::str_extract(data()$name, "[a-z|A-Z]")))) {
            lon <- input[[paste0(id_map, "_click")]]$coordinate[[1]]
            lat <- input[[paste0(id_map, "_click")]]$coordinate[[2]]
            name <- curbcut::rev_geocode(lon = lon, lat = lat)
            
            scale <- 
              curbcut::cc_t(lang = r$lang(), translation = translation, 
                            scales_dictionary$sing[scales_dictionary$scale == df()])
            
            curbcut::cc_t(lang = r$lang(), translation = translation, 
                          "The {scale} around '{name}'")
          } else {
            data()$name[data()$ID == r[[id]]$select_id()]
          }
        
        title_card_data <- curbcut::placeex_main_card(pe_main_card = pe_main_card,
                                                      region = r$region(),
                                                      df = df(),
                                                      select_id = r[[id]]$select_id(),
                                                      lang = r$lang(),
                                                      translation = translation)
        # title_card_data <- curbcut::placeex_main_card(pe_main_card = pe_main_card,
        #                                               region = "CMA",
        #                                               df = "DA",
        #                                               select_id = "24520101",
        #                                               lang = "en",
        #                                               translation = translation)
        
        map_zoom <- 
          get(paste("map_zoom_levels", r$region(), sep = "_"))[[
            gsub(".*_", "", df())]] + 0.75
        scale_sing <- scales_dictionary$slider_title[
          scales_dictionary$scale == df()]
        map_loc <- data()$centroid[data()$ID == r[[id]]$select_id()][[1]]
        
        ## Create the curbcut:: package and start with the place_explorer stuff
          rmarkdown::render("www/place_explorer.Rmd", params = list(
          select_id = r[[id]]$select_id(),
          title = title,
          region = r$region(),
          df = df(),
          scale_sing = curbcut::cc_t("{scale_sing} (count)", lang = r$lang(),
                                     translation = translation),
          map_loc = map_loc,
          map_zoom = map_zoom,
          mapbox_username = mapbox_username,
          title_card_data = title_card_data
        ), envir = new.env())
       
          x <- readLines("www/place_explorer.html")
          x <-
            x[-((str_detect(x, "<head") |> which()):(str_detect(x, "</head") |> which()))]
          writeLines(x, "www/place_explorer.html")
          
          head <- readLines("www/place_explorer_head.html")
          x <- c(head, x)
          
          writeLines(x, "www/place_explorer_temp.html")
          

        div(class = "main_panel_popup", 
            style = "height:100%;overflow:hidden;",
            tags$iframe(style = "width:100%;height:100%;", 
                        src = "place_explorer_temp.html", 
                        frameborder = 0)
        )
      }
    })
    
  })
}
