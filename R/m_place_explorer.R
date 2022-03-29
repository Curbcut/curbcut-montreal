### PLACE EXPLORER MODULE ######################################################

# UI ----------------------------------------------------------------------

place_explorer_UI <- function(id) {
  ns_id <- "place_explorer"
  
  fillPage(fillRow(fillCol(
    
    # Temporary fix to clicking the enter key is as clicking on Search button
    tags$script('$(function() {
      var $els = $("[data-proxy-click]");
                       $.each(
                         $els,
                         function(idx, el) {
                           var $el = $(el);
                           var $proxy = $("#" + $el.data("proxyClick"));
                           $el.keydown(function (e) {
                             if (e.keyCode == 13) {
                               $proxy.click();
                             }
                           });
                         }
                       );
                     });
                     '),
    `data-proxy-click` = "place_explorer-search_button",
    
    # Main map
    div(id = NS(id, "mapdeck_div"),
      class = "mapdeck_div",
      rdeckOutput(NS(id, paste0(ns_id, "-map")), height = "100%")),
    
    # Sidebar
    sidebar_UI(
      NS(id, "place_explorer"),
      hidden(actionLink(inputId = NS(id, "comeback_map"),
                        label = sus_translate("Go back to map"),
                        style = "font-size: 1.25rem;")),
      
      susSidebarWidgets(
        # Search box
        strong(sus_translate("Enter a postal code or click on the map")),
        HTML(paste0('
                   <div class="shiny-split-layout">
                     <div style="width: 60%;">',
                    textInput(inputId = NS(id, "adress_searched"), label = NULL,
                              placeholder = "H3A 2T5"),
                    '</div>
                     <div style="width: 40%;">',
                    actionButton(inputId = NS(id, "search_button"),
                                 label = "Search",
                                 style = "margin-top: var(--padding-v-md);"),
                    '</div>
                     </div>'))),
      
      hidden(div(id = NS(id, "sidebar_widgets"), susSidebarWidgets(
        
        # Checkboxes of each theme
        pickerInput(
          inputId = NS(id, "themes_checkbox"),
          label = sus_translate("Select theme(s):"),
          choices = unique(variables$theme),
          selected = unique(variables$theme),
          options = list(`selected-text-format` = "count > 4"),
          multiple = TRUE),
        
        br(),
        
        # Retrieve the scale the user is interested in
        sliderTextInput(inputId = NS(id, "slider"),
                        label = sus_translate("Select scale:"),
                        choices = c("Borough/city", "Census tract", 
                                    "Dissemination area"),
                        selected = "Dissemination area",
                        hide_min_max = TRUE, 
                        force_edges = TRUE),
        
        # Island only comparison, or region-wide
        pickerInput(
          inputId = NS(id, "comparison_scale"),
          label = sus_translate("Choose comparison scale:"),
          choices = list("Island" = "island",
                      "Region" = "region"))
      )))),
    
    # Main panel as a uiOutput. The number of themes displayed is reactive
    fluidPage(
      hidden(div(id = NS(id, "grid_elements"),
                 style = paste0("margin-top:25px; overflow-x: hidden; ",
                                "overflow-y: auto;  height: calc(100vh - 105px);",
                                "margin-left:310px; background-color:#ffffff;",
                                "padding:25px;"),
                 fluidRow(
                   style = paste0("font-size: 11px;",
                                  "max-width: 100%; margin:auto; background-color:#fbfbfb;",
                                  "padding:30px; border: 1px solid #00000030;"),
                   column(9, htmlOutput(NS(id, "title_card_title")),
                          uiOutput(NS(id, "title_card"), style = "margin-top:20px;")),
                   column(3, rdeckOutput(NS(id, "title_card_map")))
                 ),
                 
                 fluidRow(uiOutput(NS(id, "themes_grid")))))),
  )))
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "place_explorer"
    
    # Reactive values initialization
    loc_DAUID <- reactiveVal()
    location_name <- reactiveVal()
    island_comparison <- reactiveVal("island")
    
    # Sidebar
    sidebar_server(id = "place_explorer", x = "place_explorer")
    
    ## Picker input translation ------------------------------------------
    
    observe({
      all_themes <- unique(variables$theme)
      names(all_themes) <- sapply(all_themes, sus_translate, USE.NAMES = FALSE)
      
      updatePickerInput(
        session = session,
        inputId = "themes_checkbox",
        choices = all_themes,
        selected = all_themes)
    })
    
    ## MAIN MAP ----------------------------------------------------------
    output[[paste0(ns_id, "-map")]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = map_zoom)) |> 
        add_mvt_layer(
          id = "ghost_DA", 
          data = tile_json("maxbdb2.place_explorer-DA2"),
          pickable = TRUE,
          get_fill_color = "#FFFFFF00",
          get_line_color = "#FFFFFF00")
    })
    
    observeEvent(input[[paste0(ns_id, "-map_viewstate")]]$viewState$zoom, {
      if (input[[paste0(ns_id, "-map_viewstate")]]$viewState$zoom < 8) {
        rdeck_proxy(id = paste0(ns_id, "-map")) |>
          add_polygon_layer(data = place_explorer_basemap,
                            id = "border",
                            get_fill_color = "#AAB6CF",
                            get_polygon = rlang::sym("x"))
      } else {
        rdeck_proxy(id = paste0(ns_id, "-map")) |>
          add_polygon_layer(data = place_explorer_basemap,
                            id = "border",
                            get_fill_color = "#AAB6CF00",
                            get_line_color = "#AAB6CFFF",
                            line_width_units = "pixels",
                            get_line_width = 3,
                            get_polygon = rlang::sym("x"))
      }
    }, ignoreInit = TRUE)
    
    ## RETRIEVE LOCATION ------------------------------------------------
    # Get point data from a search
    observeEvent(input$search_button, {
      
      postal_c <- str_to_lower(input$adress_searched) |>
        str_extract_all("\\w|\\d", simplify = TRUE) |>
        paste(collapse = "")
      
      if (postal_c %in% postal_codes$postal_code) {
        loc_DAUID(postal_codes$DAUID[postal_codes$postal_code == postal_c])
        
        location_name(
          postal_codes$postal_code[postal_codes$postal_code == postal_c] |>
            str_to_upper() |>
            str_replace_all("(.{3})", "\\1 ") |>
            str_trim())
        
      } else {
        showNotification(
          paste0("No postal code found for `", input$adress_searched, "`"),
          type = "error")
      }
      
    })
    
    # Get point data from a click
    observeEvent(input[[paste0(ns_id, "-map_click")]], {
      loc_DAUID(input[[paste0(ns_id, "-map_click")]]$object$DAUID)
      
      lon <- input[[paste0(ns_id, "-map_click")]]$coordinate[[1]]
      lat <- input[[paste0(ns_id, "-map_click")]]$coordinate[[2]]
      
      name <- tmaptools::rev_geocode_OSM(x = lon, y = lat)[[1]]
      
      town_city_county <-
        if (!is.null(name$town)) {
          name$town
        } else if (!is.null(name$city)) {
          name$city
        } else if (!is.null(name$county)) {
          name$county
        }
      
      location_name(paste0(
        name$house_number, " ",
        name$road, ", ",
        # One or the other:
        town_city_county))
      
    })
    
    observeEvent(input[["title_card_map_click"]], {
      loc_DAUID(input[["title_card_map_click"]]$object$DAUID)
      
      lon <- input[["title_card_map_click"]]$coordinate[[1]]
      lat <- input[["title_card_map_click"]]$coordinate[[2]]
      
      name <- tmaptools::rev_geocode_OSM(x = lon, y = lat)[[1]]
      
      town_city_county <-
        if (!is.null(name$town)) {
          name$town
        } else if (!is.null(name$city)) {
          name$city
        } else if (!is.null(name$county)) {
          name$county
        }
      
      location_name(paste0(
        name$house_number, " ",
        name$road, ", ",
        # One or the other:
        town_city_county))
      
    })
    
    ## MAIN MAP UPDATES AND JS ------------------------------------------
    widgets_name <- c("gridelements", "comeback_map",
                      "grid_elements", "sidebar_widgets")
    
    observeEvent(loc_DAUID(), {
      lapply(widgets_name, \(x) hide(x, anim = TRUE, animType = "fade", time = 0.5))
      lapply(widgets_name, \(x) show(x, anim = TRUE, animType = "fade", time = 0.5))
      hide("mapdeck_div", anim = TRUE, animType = "fade", time = 0.5)
    }, ignoreInit = TRUE)
    
    # Hook up the go back to map
    observeEvent(input$comeback_map, {
      lapply(widgets_name, \(x) hide(x, anim = TRUE, animType = "fade", time = 0.5))
      show("mapdeck_div", anim = TRUE, animType = "fade", time = 0.5)
    })
    
    
    ## RETRIEVE df AND ROW ID ------------------------------------------
    observe({
      loc_DAUID()
      updateSliderTextInput(session = session,
                            "slider",
                            choices = get_zoom_label_t(map_zoom_levels[1:3]),
                            selected = get_zoom_label_t(map_zoom_levels[1:3])[3])
    })
    
    df <- reactive(get_zoom_code(input$slider))
    data <- reactive(get(get_zoom_code(input$slider)))
    
    # Depending on `df`, retrieve the ID.
    select_id <- eventReactive({
      df()
      loc_DAUID()}, {
        to_retrieve <- 
          switch(df(),
                 "borough" = "CSDUID",
                 "CT" = "CTUID",
                 "DA" = "DAUID")
        DA[[to_retrieve]][DA$DAUID == loc_DAUID()]
      }, ignoreInit = TRUE)

    ## ISLAND OR REGION COMPARISON -------------------------------------
    
    # Reactive to toggle on or off the presence of the island_region widget
    location_on_island <- eventReactive(select_id(), {
      data()$CSDUID[data()$ID == select_id()] %in% island_CSDUID
    }, ignoreInit = TRUE)
    
    # Should we show the wdiget, or not? Only if loc_DAUID() is on island
    observe(toggle(id = "comparison_scale", condition = location_on_island()))
    
    observe({
      updatePickerInput(session,
                        inputId = "comparison_scale",
                        choices = sus_translate(list("Island" = "island",
                                                     "Region" = "region")),
                        selected = if (location_on_island()) "island" else "region")
    })
    
    # Every time the selected id changes, re-evaluate if we're starting with
    # an island-only comparison, or region-wide.
    observeEvent(location_on_island(), {
        island_comparison(if (location_on_island()) "island" else "region")
    }, ignoreInit = TRUE)
    
    observeEvent(input$comparison_scale, {
        island_comparison(input$comparison_scale)
    })
    
    ## TITLE CARD -------------------------------------------------------
    shinyjs::delay(1, shinyjs::show("grid_elements"))
    shinyjs::delay(750, shinyjs::hide("grid_elements"))

    output$title_card_map <- renderRdeck({
      rdeck(map_style = map_base_style,
            initial_view_state =
              view_state(center = map_location,
                         zoom = 14)) |>
        add_mvt_layer(
          id = "ghost_DA",
          data = tile_json("maxbdb2.place_explorer-DA2"),
          pickable = TRUE,
          get_fill_color = "#FFFFFF00",
          get_line_color = "#FFFFFF00")
    })

    observeEvent(loc_DAUID(), {
          data <- data()[data()$ID == select_id(), "geometry"]
          zoom <- map_zoom_levels[
            which(df() == names(map_zoom_levels))] + 2
          if (zoom == 2) zoom <- 11
          center <- eval(parse(text =
                                 as.character(st_centroid(data)$geometry)))
          rdeck_proxy(id = "title_card_map",
                      initial_view_state =
                        view_state(center = center,
                                   zoom = as.numeric(zoom))) |>
            add_polygon_layer(data = data,
                              id = "actual_location",
                              highlight_color = "#FFFFFF80",
                              get_fill_color = "#BAE4B3BB",
                              get_line_color = "#FFFFFF",
                              get_line_width = 5,
                              auto_highlight = TRUE,
                              get_polygon = rlang::sym("geometry"))
      }, ignoreInit = TRUE)

    output$title_card_title <- renderText({
      if (df() == "borough") {
        HTML("<h2>",
             paste0(borough[borough$ID == select_id(),]$name, " (",
                    borough[borough$ID == select_id(),]$name_2, ")"),
             "/h2>")
      } else HTML("<h2 style = 'display:inline;'>", 
                  paste0("The area around ", location_name(),
                         "<i style = 'color: var(--c-h2);
    font-family: var(--ff-h2); font-size: 2.5rem; margin-bottom: 0.75em; 
                         display:inline;'>", 
                         "&nbsp;&nbsp;&nbsp;(", get_zoom_name(df()), ")"), 
                  "</i></h2>")
    })

    output$title_card <- renderUI({
      
      output$list <- renderUI({
        title_card_to_grid <<- get_title_card(
          df(), select_id(),
          island_or_region = island_comparison())
        
        lapply(seq_along(title_card_to_grid), \(x) {
          output[[paste0("ind_", x, "_plot")]] <- renderPlot({
            title_card_to_grid[[x]][["graph"]]
          })
        })
        
        lapply(seq_along(title_card_to_grid), \(x) {
          tagList(
            fluidRow(
              column(width = 2,
                     HTML(paste0("<p style = 'margin:auto; text-align:center;",
                                 "font-size: medium; font-weight:bold;'>",
                                 title_card_to_grid[[x]][["row_title"]] |>
                                   str_to_upper(), "</p>"))),
              column(width = 2,
                     HTML(paste0("<p style = 'margin:auto; text-align:center;'>",
                                 title_card_to_grid[[x]][["percentile"]] |>
                                   str_to_upper(), "</p>"))),
              column(width = 2,
                     plotOutput(eval(parse(
                       text = paste0("NS(id, 'ind_", x, "_plot')"))),
                       height = 25)),
              column(width = 6,
                     HTML(paste0("<p style = 'color: #999999; font-size:small'>",
                                 paste0(title_card_to_grid[[x]][["text"]],
                                        title_card_to_grid[[x]][["link"]]), 
                                 "</p>")))
              
            ),
            br()
          )
        })
      })
    })

    ## PLACE EXPLORER DATA ----------------------------------------------
    output$themes_grid <- renderUI({
        themes <-
          pe_theme_order[[df()]][pe_theme_order[[df()]]$ID == select_id(), ]
        themes <-
          themes[themes$group == island_comparison(), ]

        standout <- themes$standout
        themes <- themes$theme

        standout_definition <-
          c("Extreme outlier" =
              paste0("`Extreme outlier` means that the variables comprising ",
                     "the theme, on average, \nrank  in the top or bottom 10% ",
                     "relative to the {island_comparison()}."),
            "Outlier" =
              paste0("`Outlier` means that the variables comprising the theme, ",
                     "on average, \nrank in the top or bottom 20% relative to ",
                     "the {island_comparison()}."),
            "Typical" =
              paste0("`Typical` means that the variables comprising the theme, ",
                     "on average, \nrank between the top or bottom 20% relative ",
                     "to the {island_comparison()}."))

        # The "server" of every block
        lapply(seq_along(themes), \(x) {
          delay(x*100, {
            block <- paste0("theme_", themes[[x]], "_block")
            
            output[[block]] <- renderUI({

                to_grid <- place_explorer_block_text(
                  df = df(),
                  theme = themes[[x]],
                  select_id = select_id(),
                  island_or_region = island_comparison())

                plots <- place_explorer_block_plot(
                  df = df(),
                  theme = themes[[x]],
                  select_id = select_id(),
                  island_or_region = island_comparison()
                )
                
                if (nrow(to_grid) > 0)
                  lapply(seq_len(nrow(to_grid)), \(z) {
                    output[[paste0("ind_", themes[[x]], z, "_row_title")]] <- renderText({

                      paste(p(style = "    font-size: 11px;", to_grid[z, ][["var_title"]],
                              icon("question"),
                              title = str_to_sentence(to_grid[z, ][["explanation"]])))
                    })
                    output[[paste0("ind_", themes[[x]],  z, "_percentile")]] <- renderText({
                      to_grid[z, ][["percentile"]]
                    })
                    output[[paste0("ind_", themes[[x]], z, "_value")]] <- renderText({
                      to_grid[z, ][["value"]]
                    })
                    output[[paste0("ind_", themes[[x]], z, "_plot")]] <- renderPlot({
                      plots[[z]]
                    })
                  })
                
                if (nrow(to_grid) > 0) {
                  translated_theme <- str_to_upper(sus_translate(themes[[x]]))
                  translated_standout <- str_to_lower(sus_translate(standout[[x]]))
                  translated_standout_definition <-
                    sus_translate(standout_definition[[which(names(standout_definition) == standout[[x]])]])

                  nb_values_to_show <- min(nrow(to_grid), 5)

                  block_title <- paste0(translated_theme, " (", translated_standout, ")")

                  tagList(h3(style = "text-transform:inherit;",
                             block_title,
                             title = translated_standout_definition),
                          lapply(seq_len(nb_values_to_show), \(z) {
                            tagList(
                              fluidRow(
                                column(width = 4,
                                       if (z == 1) h5(sus_translate("Variable")),
                                       htmlOutput(eval(parse(
                                         text = paste0("NS(id, 'ind_", themes[[x]], z, "_row_title')"))))),
                                column(width = 2,
                                       if (z == 1) h5(sus_translate("Rank")),
                                       htmlOutput(eval(parse(
                                         text = paste0("NS(id, 'ind_", themes[[x]], z, "_percentile')"))))),
                                column(width = 2,
                                       if (z == 1) h5(sus_translate("Value")),
                                       htmlOutput(eval(parse(
                                         text = paste0("NS(id, 'ind_", themes[[x]], z, "_value')"))))),
                                column(width = 3,
                                       if (z == 1) h5(sus_translate("Plot")),
                                       plotOutput(eval(parse(
                                         text = paste0("NS(id, 'ind_", themes[[x]], z, "_plot')"))),
                                         height = 25))
                              ),
                              br()
                            )
                          })
                  )} else {
                    tagList(fluidRow(h3(sus_translate(themes[[x]]))),
                            fluidRow("No data."))
                  }
            })
          })
        })

        which_standout <- which(standout %in% c("Extreme outlier", "Outlier"))

        lapply(seq_along(themes), \(x) {
          tagList(
            if (x == 1 && length(which_standout) != 0) {
              tagList(h2(style = "padding: 10px;",
                         sus_translate("What makes this area unique?")))
            } else if ((x == 1 && length(which_standout) == 0) ||
                       (length(which_standout) != 0 && 
                       x - 1 == which_standout[length(which_standout)])) {
              tagList(h2(style = "padding: 10px;",
                         sus_translate("What makes this area similar to others?")))
            },
            uiOutput(
              outputId = eval(parse(text = paste0("NS(id, 'theme_", themes[[x]], "_block')"))),
              style = paste0("padding: 20px; margin: 10px; font-size: 11px;",
                             "display: inline-grid; width: 48%;"),
              class = "panel panel-default "))
        })
    })

    observeEvent(input$themes_checkbox, {
        themes <-
          pe_theme_order[[df()]][pe_theme_order[[df()]]$ID == select_id(), ]
        themes <-
          themes[themes$group == island_comparison(), ]
        themes <- themes$theme

        to_hide <- themes[!themes %in% input$themes_checkbox]
        to_show <- themes[themes %in% input$themes_checkbox]

        lapply(to_hide, \(x) {
          hide(paste0("theme_", x, "_block"))
        })
        lapply(to_show, \(x) {
          show(paste0("theme_", x, "_block"))
        })
    })

    observeEvent(input$title_card_total_crash_per1k, {
      z <- title_card_to_grid[["total_crash_per1k"]]
      module_link(module = z$link_module,
                  select_id = select_id(),
                  var_left = z$link_var_left,
                  df = df())
    })

    observeEvent(input$title_card_single_detached, {
      z <- title_card_to_grid[["single_detached"]]
      module_link(module = z$link_module,
                  select_id = select_id(),
                  var_left = z$link_var_left,
                  df = df())
    })

    observeEvent(input$title_card_green_space_ndvi, {
      z <- title_card_to_grid[["green_space_ndvi"]]
      module_link(module = z$link_module,
                  select_id = select_id(),
                  var_left = z$link_var_left,
                  df = df())
    })

    observeEvent(input$title_card_canale_index, {
      z <- title_card_to_grid[["canale_index"]]
      module_link(module = z$link_module,
                  select_id = select_id(),
                  var_left = z$link_var_left,
                  df = df())
    })
  })
}

