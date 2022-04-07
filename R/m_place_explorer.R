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
                     <div style="width: 80%;">',
                    textInput(inputId = NS(id, "address_searched"), 
                              label = NULL, placeholder = "H3A 2T5"),
                    '</div>
                     <div style="width: 20%;">',
                    actionButton(inputId = NS(id, "search_button"),
                                 label = icon("search"),
                                 style = "margin-top: var(--padding-v-md);"),
                    '</div>
                     </div>'))),
      
      hidden(div(id = NS(id, "sidebar_widgets"), susSidebarWidgets(
        
        # Checkboxes for each theme
        pickerInput(
          inputId = NS(id, "themes_checkbox"),
          label = sus_translate("Select theme(s):"),
          choices = unique(variables$theme),
          selected = unique(variables$theme),
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
        
        # Island-only or region-wide comparison
        select_var_UI(
          id = ns_id,
          select_var_id = NS(id, "comparison_scale"),
          label = sus_translate("Choose comparison scale:"),
          var_list = list("Island" = "island", "Region" = "region"))
      )))),
    
    # Main panel as a uiOutput. The number of themes displayed is reactive
    fluidPage(
      hidden(div(id = NS(id, "grid_elements"),
                 style = paste0(
                   "margin-top:25px; overflow-x: hidden; ",
                   "overflow-y: auto;  height: calc(100vh - 105px);",
                   "margin-left:310px; background-color:#ffffff;",
                   "padding:25px;"),
                 
                 fluidRow(
                   style = paste0(
                     "font-size: 11px;",
                     "max-width: 100%; margin:auto; background-color:#fbfbfb;",
                     "padding:30px; border: 1px solid #00000030;"),
                   column(9, htmlOutput(NS(id, "title_card_title")),
                          uiOutput(NS(id, "title_card"), 
                                   style = "margin-top:20px;")),
                   column(3, rdeckOutput(NS(id, "title_card_map")))),
                 
                 fluidRow(uiOutput(NS(id, "themes_grid")))))),
  )))
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "place_explorer"
    
    # Inputs and reactives -----------------------------------------------------
    
    # Initial reactives
    zoom <- reactiveVal(get_zoom(map_zoom))
    loc_DAUID <- reactiveVal()
    location_name <- reactiveVal()
    island_comparison <- reactiveVal("island")
    
    # Zoom reactive
    observeEvent(input[[paste0(ns_id, "-map_viewstate")]], {
      zoom(get_zoom(input[[paste0(ns_id, "-map_viewstate")]]$viewState$zoom))
    })
    
    # Sidebar
    sidebar_server(id = "place_explorer", x = "place_explorer")
    
    # Picker input translation
    observe({
      all_themes <- unique(variables$theme)
      names(all_themes) <- sapply(all_themes, sus_translate, USE.NAMES = FALSE)
      
      updatePickerInput(
        session = session,
        inputId = "themes_checkbox",
        choices = all_themes,
        selected = all_themes)
    })
    
    
    # Main map -----------------------------------------------------------------
    
    output[[paste0(ns_id, "-map")]] <- renderRdeck({
      rdeck(map_style = map_base_style, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom)) |> 
        add_mvt_layer(
          id = "CMA",
          data = mvt_url("sus-mcgill.CMA_empty"),
          get_fill_color = "#AAB6CF20",
          get_line_color = "#AAB6CF00",
          line_width_units = "pixels",
          get_line_width = 0) |> 
        add_mvt_layer(
          id = "ghost_DA", 
          data = mvt_url("sus-mcgill.DA_empty"),
          pickable = TRUE,
          auto_highlight = TRUE,
          highlight_color = "#AAB6CF60",
          get_fill_color = "#FFFFFF00",
          get_line_color = "#FFFFFF00")
    })
    
    observeEvent(zoom(), {
      rdeck_proxy(id = paste0(ns_id, "-map")) |>
        add_mvt_layer(id = "CMA", 
                      get_fill_color = if (zoom() >= 8) 
                        "#AAB6CF20" else "#AAB6CFFF")
    })
    
    
    # Retrieve location --------------------------------------------------------
    
    # Get point data from search
    observeEvent(input$search_button, {
      
      postal_c <- str_to_lower(input$address_searched) |>
        str_extract_all("\\w|\\d", simplify = TRUE) |>
        paste(collapse = "")
      
      pcs <- postal_codes$postal_code == postal_c
      
      if (sum(pcs) > 0) {
        loc_DAUID(postal_codes$DAUID[pcs])
        
        location_name(
          postal_codes$postal_code[pcs] |>
            str_to_upper() |>
            (\(x) paste(substr(x, 1, 3), substr(x, 4, 6)))())
        
      } else {
        showNotification(
          paste0("No postal code found for `", input$address_searched, "`"),
          type = "error")
      }
      
    })
    
    
    # Get point data from click ------------------------------------------------
    
    # Click on map
    observeEvent(input[[paste0(ns_id, "-map_click")]], {
      loc_DAUID(input[[paste0(ns_id, "-map_click")]]$object$ID)
      
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
      
      location_name(paste0(name$house_number, " ", name$road, ", ", 
                           town_city_county)) # One or the other
      
    })
    
    # Click on title card map
    observeEvent(input[["title_card_map_click"]], {
      loc_DAUID(input[["title_card_map_click"]]$object$ID)
      
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
      
      location_name(paste0(name$house_number, " ", name$road, ", ",
                           town_city_county)) # One or the other
      
    })
    
    
    # Main map updates and JS --------------------------------------------------
    
    widgets_name <- c("gridelements", "comeback_map", "grid_elements", 
                      "sidebar_widgets")
    
    observeEvent(loc_DAUID(), {
      lapply(widgets_name, hide, anim = TRUE, animType = "fade", time = 0.5)
      lapply(widgets_name, show, anim = TRUE, animType = "fade", time = 0.5)
      hide("mapdeck_div", anim = TRUE, animType = "fade", time = 0.5)
    }, ignoreInit = TRUE)
    
    # Hook up the 'go back to map'
    observeEvent(input$comeback_map, {
      lapply(widgets_name, hide, anim = TRUE, animType = "fade", time = 0.5)
      show("mapdeck_div", anim = TRUE, animType = "fade", time = 0.5)
    })
    
    
    # Retrieve df and row ID ---------------------------------------------------
    
    observe(updateSliderTextInput(
      session = session, "slider",
      choices = get_zoom_label_t(map_zoom_levels[1:3])))
    
    df <- reactive(get_zoom_code(input$slider))
    data <- reactive(get(get_zoom_code(input$slider)))
    
    # Depending on `df`, retrieve the ID.
    select_id <- reactive({
      to_retrieve <- switch(df(), "borough" = "CSDUID", "CT" = "CTUID",
                            "DA" = "DAUID")
      DA[[to_retrieve]][DA$DAUID == loc_DAUID()]}) |> 
      bindEvent(df(), loc_DAUID(), ignoreInit = TRUE)
    
    
    # Island or region comparison ----------------------------------------------
    
    # Reactive to toggle on or off the presence of the island_region widget
    location_on_island <- reactive(
      data()$CSDUID[data()$ID == select_id()] %in% island_CSDUID) |> 
      bindEvent(select_id())
    
    # Should we show the widget, or not? Only if loc_DAUID() is on island
    observe(toggle(id = "comparison_scale", condition = location_on_island()))
    
    # Update dropdown language
    comparison_scale <- select_var_server(
      id = ns_id, 
      select_var_id = "comparison_scale",
      var_list = reactive(list("Island" = "island", "Region" = "region")))
    
    # Every time select_id changes, reevaluate if we're starting with
    # an island-only or region-wide comparison
    island_comparison <- reactive(if (!location_on_island()) 
      "region" else comparison_scale())
    
    
    # Title card ---------------------------------------------------------------
    
    shinyjs::delay(1, shinyjs::show("grid_elements"))
    shinyjs::delay(750, shinyjs::hide("grid_elements"))
    
    output$title_card_map <- renderRdeck({
      rdeck(map_style = map_base_style,
            initial_view_state = view_state(center = map_loc, zoom = 14)) |>
        add_mvt_layer(
          id = "ghost_DA",
          data = mvt_url("sus-mcgill.DA_empty"),
          pickable = TRUE,
          get_fill_color = "#FFFFFF00",
          get_line_color = "#FFFFFF00")
    })
    
    
    observeEvent(select_id(), {
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
    })
    
    output$title_card_title <- renderText({
      if (df() == "borough") {
        HTML("<h2>",
             paste0(borough[borough$ID == select_id(),]$name, 
                    "<i style = 'color: var(--c-h2);
    font-family: var(--ff-h2); font-size: 2.5rem; margin-bottom: 0.75em; 
                         display:inline;'>", 
                    "&nbsp;&nbsp;&nbsp;(", 
                    borough[borough$ID == select_id(),]$name_2, 
                    ")"), 
             "</i></h2>")
      } else HTML("<h2 style = 'display:inline;'>", 
                  paste0(sus_translate("The area around "), location_name(),
                         "<i style = 'color: var(--c-h2);
    font-family: var(--ff-h2); font-size: 2.5rem; margin-bottom: 0.75em; 
                         display:inline;'>", 
                         "&nbsp;&nbsp;&nbsp;(", 
                         sus_translate(get_zoom_name(df())), ")"), 
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
              column(width = 2, HTML(paste0(
                "<p style = 'margin:auto; text-align:center;",
                "font-size: medium; font-weight:bold;'>",
                sus_translate(title_card_to_grid[[x]][["row_title"]]) |>
                  str_to_upper(), "</p>"))),
              column(width = 2, HTML(paste0(
                "<p style = 'margin:auto; text-align:center;'>",
                title_card_to_grid[[x]][["percentile"]] |>
                  str_to_upper(), "</p>"))),
              column(width = 2, plotOutput(eval(parse(
                text = paste0("NS(id, 'ind_", x, "_plot')"))), height = 25)),
              column(width = 6, HTML(paste0(
                "<p style = 'color: #999999; font-size:small'>",
                paste0(title_card_to_grid[[x]][["text"]],
                       title_card_to_grid[[x]][["link"]]), 
                "</p>")))
            ),
            br()
          )
        })
      })
    })
    
    ## Place explorer data -----------------------------------------------------
    
    output$themes_grid <- renderUI({
      themes <-
        pe_theme_order[[df()]][pe_theme_order[[df()]]$ID == select_id(), ]
      themes <-
        themes[themes$group == island_comparison(), ]
      
      standout <- themes$standout
      themes <- themes$theme
      
      text_island_region <- 
        if (island_comparison() == "island") {
          sus_translate("the island")
        } else {
          sus_translate("the region")
        }
      
      standout_definition <-
        c("Extreme outlier" =
            sus_translate("`Extreme outlier`: the variables rank in the top/bottom ",
                          "10% relative to {text_island_region}."),
          "Outlier" =
            sus_translate("`Outlier`: the variables rank in the top/bottom 20% ",
                          "relative to {text_island_region}."),
          "Typical" =
            sus_translate("`Typical`: the variables rank in the middle 60% ",
                          "relative to {text_island_region}."))
      
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
            
            sentence <- place_explorer_block_sentence(
              df = df(),
              theme = themes[[x]],
              select_id = select_id(),
              island_or_region = island_comparison()
            )
            
            
            if (nrow(to_grid) > 0)
              lapply(seq_len(nrow(to_grid)), \(z) {
                output[[paste0("ind_", themes[[x]], z, "_row_title")]] <- 
                  renderText({
                    paste(p(style = "    font-size: 11px;", 
                            sus_translate(to_grid[z, ][["var_title"]]),
                            icon("question"),
                            title = str_to_sentence(
                              sus_translate(to_grid[z, ][["explanation"]]))))
                  })
                
                output[[paste0("ind_", themes[[x]],  z, "_percentile")]] <- 
                  renderText({to_grid[z, ][["percentile"]]})
                
                output[[paste0("ind_", themes[[x]], z, "_value")]] <- 
                  renderText({to_grid[z, ][["value"]]})
                
                output[[paste0("ind_", themes[[x]], z, "_plot")]] <- 
                  renderPlot({plots[[z]]})
                
              })
            
            if (nrow(to_grid) > 0) {
              translated_theme <- 
                str_to_upper(sus_translate(themes[[x]]))
              translated_standout <- 
                str_to_lower(sus_translate(standout[[x]]))
              translated_standout_definition <-
                standout_definition[[which(names(
                  standout_definition) == standout[[x]])]]
              
              nb_values_to_show <- min(nrow(to_grid), 5)
              
              block_title <- paste0(translated_theme, " (", 
                                    translated_standout, ")")
              
              tagList(
                h3(style = "text-transform:inherit;",
                   block_title, 
                   title = translated_standout_definition),
                if (!is.null(sentence)) p(style = "font-size: small;",
                                          sentence),
                lapply(seq_len(nb_values_to_show), \(z) {
                  tagList(fluidRow(
                    
                    column(width = 4, 
                           if (z == 1) h5(sus_translate("Variable")),
                           htmlOutput(eval(parse(
                             text = paste0("NS(id, 'ind_", themes[[x]], z, 
                                           "_row_title')"))))),
                    
                    column(width = 2,
                           if (z == 1) h5(sus_translate("Rank")),
                           htmlOutput(eval(parse(
                             text = paste0("NS(id, 'ind_", themes[[x]], z, 
                                           "_percentile')"))))),
                    
                    column(width = 2,
                           if (z == 1) h5(sus_translate("Value")),
                           htmlOutput(eval(parse(
                             text = paste0("NS(id, 'ind_", themes[[x]], z, 
                                           "_value')"))))),
                    
                    column(width = 3,
                           if (z == 1) h5(sus_translate("Plot")),
                           plotOutput(eval(parse(
                             text = paste0("NS(id, 'ind_", themes[[x]], z, 
                                           "_plot')"))),
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
                       sus_translate(
                         "What makes this area similar to others?")))
          },
          uiOutput(
            outputId = eval(parse(text = paste0("NS(id, 'theme_", themes[[x]], 
                                                "_block')"))),
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

