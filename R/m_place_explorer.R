### PLACE EXPLORER MODULE ######################################################

# UI ----------------------------------------------------------------------

place_explorer_UI <- function(id) {
  ns_id <- "place_explorer"
  
  fillPage(fillRow(fillCol(

    # Style
    inlineCSS("#deckgl-overlay { z-index:4; }"),
    inlineCSS(list(.big_map = "width: 100%; height: 100vh; display:visible;")),
    inlineCSS(list(.banner_map = "display:none;")),#"width: 100%; height: 125px;")),
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
    div(
      id = NS(id, "mapdeck_div"),
      class = "big_map",
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
          label = "Select theme(s):",
          choices = unique(variables$theme),
          selected = unique(variables$theme),
          options = list(`selected-text-format` = "count > 4"),
          multiple = TRUE),

        br(),

        # Retrieve the scale the user is interested in
        HTML(paste0('<label id = "', NS(id, "scalemap_label"),
                    '" class = "control-label">',
                    sus_translate('Select scale'), ':</label>')),
        rdeckOutput(NS(id, "scalemap"), height = 150),
        sliderTextInput(inputId = NS(id, "slider"),
                        label = NULL,
                        choices = c("borough", "CT", "DA"),
                        selected = "DA",
                        hide_min_max = TRUE, 
                        force_edges = TRUE),

        # Island only comparison, or region-wide
        HTML(paste0('<label id = "', NS(id, "comparison_label"),
                    '" class = "control-label">',
                    sus_translate('Choose comparison scale'), ':</label>')),
        rdeckOutput(NS(id, "island_region"), height = 100),
        htmlOutput(outputId = NS(id, "actual_comparison_scale"),
                   style = "display:none;")

      )))),

    # Main panel as a uiOutput. The number of themes displayed is reactive
    fluidPage(
      hidden(div(id = NS(id, "grid_elements"),
                 style = paste0("margin-top:25px; overflow-x: hidden; ",
                                "overflow-y: auto;  height: calc(100vh - 105px);",
                                "margin-left:310px; background-color:#ffffff;",
                                "padding:25px;"),
                 fluidRow(
                   style = paste0("padding: 5px;",
                                  "font-size: 11px;",
                                  "max-width: 1200px; margin:auto;",
                                  "padding:30px;"),
                   column(9, htmlOutput(NS(id, "title_card_title")),
                          uiOutput(NS(id, "title_card"))),
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


    ## MAIN MAP ----------------------------------------------------------
    output[[paste0(ns_id, "-map")]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_location, zoom = map_zoom)) |> 
        add_mvt_layer(
          id = "ghost_DA", 
          data = mvt_url("maxbdb2.place_explorer-DA2"),
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
      
      location_name("ADDRESS PLACE HOLDER")

    })

    observeEvent(input[["title_card_map_click"]], {
      
      loc_DAUID(input[["title_card_map_click"]]$object$DAUID)
      
      location_name("ADDRESS PLACE HOLDER")
    })

    ## MAIN MAP UPDATES AND JS ------------------------------------------
    widgets_name <- c("gridelements", "comeback_map",
                      "grid_elements", "sidebar_widgets")

    observeEvent(loc_DAUID(), {
      lapply(widgets_name, \(x) hide(x, anim = TRUE, animType = "fade", time = 0.5))
      lapply(widgets_name, \(x) show(x, anim = TRUE, animType = "fade", time = 0.5))
      removeCssClass(id = "mapdeck_div", class = "big_map")
      addCssClass(id = "mapdeck_div", class = "banner_map")
    }, ignoreInit = TRUE)

    # Hook up the go back to map
    observeEvent(input$comeback_map, {
      removeCssClass(id = "mapdeck_div", class = "banner_map")
      addCssClass(id = "mapdeck_div", class = "big_map")
      lapply(widgets_name, \(x) hide(x, anim = TRUE, animType = "fade", time = 0.5))
    })


    ## RETRIEVE df AND ROW ID ------------------------------------------
    # Reactive map depending on location
    output$scalemap <- renderRdeck({
      update_scale_map(id_map = NS(id, "scalemap"), loc_DAUID = loc_DAUID(),
                       init = TRUE)
    })
    observeEvent(loc_DAUID(), {
      update_scale_map(id_map = NS(id, "scalemap"), loc_DAUID = loc_DAUID(),
                       init = FALSE)
    })
    
    observe({
      # Placing the following as a triggering event. sus_rv$lang() is also
      # a triggering event from its presence in get_zoom_label(), and
      # for the sake of translated labels, a click on the language button
      # will change the value of the slider input.
      loc_DAUID()
      
      updateSliderTextInput(session = session,
                            "slider",
                            choices = get_zoom_label(map_zoom_levels[1:3]),
                            selected = get_zoom_label(map_zoom_levels[1:3])[3])
    })
      

    # Change df on scalemap click
    observeEvent(input[["scalemap_click"]], {
      select_df <- input[["scalemap_click"]]$layer$id
      # A click on the map triggers a change in the slider, domino to `df`
      updateSliderTextInput(session = session,
                            inputId = "slider",
                            selected = get_zoom_name(select_df))
    })

    df <- reactive({
      out <- unlist(get_zoom_code(input$slider))
      if (length(out) == 0) return(NULL) else return(out)
    })
    
    data <- reactive({
      out <- unlist(get_zoom_code(input$slider))
      if (length(out) == 0) return(NULL) else return(get(out))
      })

    # Depending on `data`, retrieve the ID.
    select_id <- reactive({
      #depending on df
      if (!is.null(loc_DAUID())) {
        to_retrieve <- 
          switch(df(),
                 "borough" = "CSDUID",
                 "CT" = "CTUID",
                 "DA" = "DAUID")
        DA[[to_retrieve]][DA$DAUID == loc_DAUID()]
      } else NULL
    })

    ## ISLAND OR REGION COMPARISON -------------------------------------
    output$island_region <- renderRdeck({island_region_map()})

    # Reactive to toggle on or off the presence of the island_region wdiget
    location_on_island <- reactive({
      if (!is.null(select_id())) {
        data()$CSDUID[data()$ID == select_id()] %in% island_CSDUID
      }})

    # Should we show the map, or not? Only if loc_DAUID() is on island
    observe({
      toggle(id = "island_region", condition = location_on_island())
      toggle(id = "comparison_label", condition = location_on_island())
      toggle(id = "actual_comparison_scale", condition = location_on_island())
    })

    # Everytime the selected id changes, reevaluate if we're starting with
    # an island-only comparison, or region-wide.
    observeEvent(select_id(), {
      if (!is.null(select_id())) {
        island_comparison(if (location_on_island()) "island" else "region")
      }
    })

    # The reaction of a click on the widget's map
    observeEvent(input[["island_region_click"]], {
      island_comparison(input[["island_region_click"]]$layer$id)
    })

    # Let the user know what is the actual scale
    output$actual_comparison_scale <- renderText({
      if (!is.null(loc_DAUID())) {
        scale <- str_to_sentence(sus_translate(island_comparison()))
        sus_translate("Current scale: {scale}")}
    })


    ## TITLE CARD -------------------------------------------------------
    shinyjs::delay(1, shinyjs::show("grid_elements"))
    shinyjs::delay(100, shinyjs::hide("grid_elements"))

    output$title_card_map <- renderRdeck({
      rdeck(map_style = map_base_style, 
            initial_view_state = 
              view_state(center = map_location,
                         zoom = 14)) |> 
        add_mvt_layer(
          id = "ghost_DA", 
          data = mvt_url("maxbdb2.place_explorer-DA2"),
          pickable = TRUE,
          get_fill_color = "#FFFFFF00",
          get_line_color = "#FFFFFF00")
    })

    observeEvent({loc_DAUID()
      data()}, {

        if (!is.null(loc_DAUID())) {
          data <- data()[data()$ID == select_id(), "geometry"]
          
          get_zoom_code(input$slider)
          zoom <- map_zoom_levels[
            which(get_zoom_code(input$slider) == names(map_zoom_levels))] + 2
          if (zoom == 2) zoom <- 11
          
          rdeck_proxy(id = "title_card_map",
                      initial_view_state = 
                        view_state(center = 
                                     eval(parse(text = 
                                                  as.character(st_centroid(data)$geometry))),
                                   zoom = as.numeric(zoom))) |>
            add_polygon_layer(data = data,
                              id = "actual_location",
                              highlight_color = "#FFFFFF80",
                              get_fill_color = "#BAE4B3BB",
                              get_line_color = "#FFFFFF",
                              get_line_width = 5,
                              auto_highlight = TRUE,
                              get_polygon = rlang::sym("geometry"))
        }
      }, ignoreInit = TRUE)

    output$title_card_title <- renderText({
      if (!is.null(df()) && !is.null(select_id()) && !is.null(loc_DAUID())) {
        HTML("<h2>",
             if (df() == "borough") {
               borough[borough$ID == select_id(),]$name
             } else location_name(),
             "</h2>")
      } else HTML("<h2>Your selected location</h2>")
    })

    output$title_card <- renderUI({

      output$list <- renderUI({
        if (!is.null(data()) && !is.null(select_id()) && !is.null(loc_DAUID())) {

          title_card_to_grid <<- get_title_card(
            data(), df(), select_id(),
            island_or_region = island_comparison())

          lapply(seq_along(title_card_to_grid), \(x) {
            output[[paste0("ind_", x, "_row_title")]] <- renderText({
              title_card_to_grid[[x]][["row_title"]] |>
                str_to_upper()
            })
            output[[paste0("ind_", x, "_percentile")]] <- renderText({
              title_card_to_grid[[x]][["percentile"]] |>
                str_to_upper()
            })
            output[[paste0("ind_", x, "_plot")]] <- renderPlot({
              title_card_to_grid[[x]][["graph"]]
            })
            output[[paste0("ind_", x, "_text")]] <- renderText({
              paste0(title_card_to_grid[[x]][["text"]],
                     title_card_to_grid[[x]][["link"]])
            })
          })

          lapply(seq_along(title_card_to_grid), \(x) {
            tagList(
              fluidRow(
                column(width = 2,
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", x, "_row_title')"))),
                         style = paste0("margin:auto; text-align:center; ",
                                        "font-size: medium; font-weight:bold;"))),
                column(width = 2,
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", x, "_percentile')"))),
                         style = paste0("margin:auto; text-align:center;"))),
                column(width = 2,
                       plotOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", x, "_plot')"))),
                         height = 25)),
                column(width = 6,
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", x, "_text')"))),
                         style = "color: #999999"))

              ),
              br()
            )
          })
        }
      })

      tagList(uiOutput(NS(id, "list")))

    })

    ## PLACE EXPLORER DATA ----------------------------------------------
    output$themes_grid <- renderUI({
      if (!is.null(df()) && !is.null(select_id()) && !is.null(loc_DAUID())) {
        
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
                lapply(1:(nrow(to_grid)), \(z) {
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
                        lapply(1:nb_values_to_show, \(z) {
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
            if (x == 1) {
              tagList(h2(style = "padding: 10px;",
                         sus_translate("Explore where the {df()} stands out")))
            } else if (x - 1 == which_standout[length(which_standout)]) {
              tagList(h2(style = "padding: 10px;",
                         sus_translate("Explore other themes")))
            },
            uiOutput(
              outputId = eval(parse(text = paste0("NS(id, 'theme_", themes[[x]], "_block')"))),
              style = paste0("padding: 20px; margin: 10px; font-size: 11px;",
                             "display: inline-grid; width: 48%;"),
              class = "panel panel-default "))
        })
      }
    })

    observeEvent(input$themes_checkbox, {
      if (!is.null(df()) && !is.null(select_id()) && !is.null(loc_DAUID())) {
        
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
      }
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
