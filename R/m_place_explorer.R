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
    data <- reactive(get(paste(r$geo(), df(), sep = "_")))
    
    # Map ---------------------------------------------------------------------

    # Main
    output[[id_map]] <- renderRdeck(
      rdeck(map_style = map_style_building, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom)) |> 
        add_mvt_layer(
          id = "place_explorer",
          name = "place_explorer",
          data = mvt_url(paste0(mapbox_username, ".", r$geo(), "_DA")),
          pickable = TRUE,
          auto_highlight = TRUE,
          highlight_color = "#AAB6CF80",
          get_fill_color = "#AAB6CF20",
          get_line_color = "#FFFFFF00")
    )
    
    # Update main map when the chosen scale changes
    rdeck_proxy(id = id_map) |>
      add_mvt_layer(id = "place_explorer",
                    data = mvt_url(paste0("sus-mcgill.", r$geo(), "_", df())),
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
            link <- paste0("photon.komoot.io/reverse?lon=", lon, "&lat=", 
                           lat)
            out <- tryCatch(httr::content(httr::GET(link, httr::timeout(2))), 
                            error = function(e) NULL)
            if (is.null(out$features) || length(out$features) == 0) {
              return(NA_character_)
            }
            out <- out$features[[1]]$properties
            third <- (function(out) {
              if (!is.null(out$city)) {
                return(gsub(" \\(\\d{2}\\)$", "", out$city))
              }
              if (!is.null(out$locality)) {
                return(out$locality)
              }
              if (!is.null(out$district)) {
                return(out$district)
              }
              if (!is.null(out$town)) {
                return(out$town)
              }
              if (!is.null(out$village)) {
                return(out$village)
              }
              if (!is.null(out$suburb)) {
                return(out$suburb)
              }
              if (!is.null(out$region)) {
                return(out$region)
              }
              if (!is.null(out$county)) {
                return(out$county)
              }
            })(out)
            second <- (function(out) {
              if (is.null(out$street)) {
                return(third)
              }
              return(paste(out$street, third, sep = ", "))
            })(out)
            name <- (function(out) {
              if (is.null(out$housenumber)) {
                return(second)
              }
              return(paste(out$housenumber, second, sep = " "))
            })(out)
            if (is.null(name)) return(NA_character_)
            
            scale <- 
              curbcut::cc_t(lang = r$lang(), translation = translation, scales_dictionary$sing[scales_dictionary$scale == df()])
            
            curbcut::cc_t(lang = r$lang(), translation = translation, "The {scale} around '{name}'")
          } else {
            data()$name[data()$ID == r[[id]]$select_id()]
          }

        
        title_card <- get_title_card(r = r, 
                                     geo = r$geo(),
                                     df = df(), 
                                     r[[id]]$select_id())
        
        map_zoom <- 
          get(paste("map_zoom_levels", r$geo(), sep = "_"))[[
            gsub(".*_", "", df())]] + 0.75
        
        ## Create the curbcut:: package and start with the place_explorer stuff

        rmarkdown::render("www/place_explorer.Rmd", params = list(
          select_id = r[[id]]$select_id(),
          title = title,
          geo = r$geo(),
          df = df(),
          map_zoom = map_zoom,
          mapbox_username = mapbox_username,
          transit_walk_cycle_share_val = title_card$transit_walk_cycle_share$percentile,
          transit_walk_cycle_share_text = title_card$transit_walk_cycle_share$text,
          transit_walk_cycle_share_colgroup = title_card$transit_walk_cycle_share$hex_cat,
          air_quality_no2_val = title_card$air_quality_no2$percentile,
          air_quality_no2_text = title_card$air_quality_no2$text,
          air_quality_no2_colgroup = title_card$air_quality_no2$hex_cat,
          single_detached_val = title_card$single_detached$percentile,
          single_detached_text = title_card$single_detached$text,
          single_detached_colgroup = title_card$single_detached$hex_cat,
          green_space_ndvi_val = title_card$green_space_ndvi$percentile,
          green_space_ndvi_text = title_card$green_space_ndvi$text,
          green_space_ndvi_colgroup = title_card$green_space_ndvi$hex_cat,
          canale_index_val = title_card$canale_index$percentile,
          canale_index_text = title_card$canale_index$text,
          canale_index_colgroup = title_card$canale_index$hex_cat
        ))

        div(class = "main_panel_popup", 
            style = "height:100%;overflow:hidden;",
            tags$iframe(style = "width:100%;height:100%;", 
                        src = "place_explorer.html", 
                        frameborder = 0)
        )
      }
    })
    
  })
}
