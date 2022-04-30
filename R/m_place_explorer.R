### PLACE EXPLORER MODULE ######################################################

# Helper function ---------------------------------------------------------

get_pe_loc <- function(name) {
  if (!is.null(name$town)) {
    name$town
  } else if (!is.null(name$city)) {
    name$city
  } else if (!is.null(name$county)) {
    name$county
  }
}


# UI ----------------------------------------------------------------------

place_explorer_UI <- function(id) {
  ns_id <- "place_explorer"
  ns_id_map <- paste0(ns_id, "-map")
  
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
        rdeckOutput(NS(id, ns_id_map), height = "100%")),
    
    # Sidebar
    sidebar_UI(
      NS(id, "place_explorer"),
      hidden(actionLink(inputId = NS(id, "back_to_map"),
                        label = sus_translate(r = r, "Go back to map"),
                        style = "font-size: 1.25rem;")),
      
      susSidebarWidgets(
        # Search box
        strong(sus_translate(r = r, "Enter postal code or click on the map")),
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
                     </div>')),
        hr(),
        # Scale slider
        sliderTextInput(inputId = NS(id, "slider"),
                        label = sus_translate(r = r, "Choose scale:"),
                        choices = c("Borough/city", "Census tract", 
                                    "Dissemination area"),
                        selected = "Dissemination area",
                        hide_min_max = TRUE, 
                        force_edges = TRUE),
      ),
      
      hidden(div(id = NS(id, "sidebar_widgets"), susSidebarWidgets(
        
        # Checkboxes for each theme
        pickerInput(
          inputId = NS(id, "themes_checkbox"),
          label = sus_translate(r = r, "Choose themes:"),
          choices = unique(variables$theme),
          selected = unique(variables$theme),
          multiple = TRUE),
        
        br(),
        
        # Island-only or region-wide comparison
        select_var_UI(
          id = ns_id,
          select_var_id = NS(id, "comparison_scale"),
          label = sus_translate(r = r, "Choose comparison scale:"),
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
                   htmlOutput(NS(id, "title_card_title")),
                   column(9,
                          uiOutput(NS(id, "title_card"), 
                                   style = "margin-top:20px;")),
                   column(3, rdeckOutput(NS(id, "title_card_map")))),
                 
                 fluidRow(uiOutput(NS(id, "themes_grid")))))),
  )))
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns_id <- "place_explorer"
    ns_id_map <- paste0(ns_id, "-map")
    ns_id_map_click <- paste0(ns_id, "-map_click")
    
    
    ## Inputs and reactives ----------------------------------------------------
    
    # Initial reactives
    loc_DAUID <- reactiveVal(NA)
    loc_name <- reactiveVal(NA)

    # Sidebar
    sidebar_server(id = "place_explorer", r = r, x = "place_explorer")
    
    # Translate picker input labels
    observe({
      all_themes <- unique(variables$theme)
      names(all_themes) <- sapply(all_themes, sus_translate, r = r, USE.NAMES = FALSE)
      updatePickerInput(
        session = session,
        inputId = "themes_checkbox",
        choices = all_themes,
        selected = all_themes)
    })
    
    # Translate slider labels
    observe(updateSliderTextInput(
      session = session, "slider",
      choices = get_zoom_label_t(map_zoom_levels[1:3], r = r)))
    
    # df, data and select_id
    df <- reactive(get_zoom_code(input$slider))
    data <- reactive(get(df()))
    select_id <- reactive({
      if (is.na(loc_DAUID())) NA else {
        to_retrieve <- switch(df(), borough = "CSDUID", CT = "CTUID", 
                              DA = "DAUID")
        DA[[to_retrieve]][DA$DAUID == loc_DAUID()]
      }
    })
    
    
    ## Main map ----------------------------------------------------------------
    
    output[[ns_id_map]] <- renderRdeck(
      rdeck(map_style = map_style_building, initial_view_state = view_state(
        center = map_loc, zoom = map_zoom)) |> 
        add_mvt_layer(
        id = "DA",
        data = mvt_url(paste0("sus-mcgill.DA_empty")),
        pickable = TRUE,
        auto_highlight = TRUE,
        highlight_color = "#AAB6CF80",
        get_fill_color = "#AAB6CF20",
        get_line_color = "#FFFFFF00")
      )
    
    
    ## Retrieve location -------------------------------------------------------

    # Search
    observe({

      postal_c <-
        input$address_searched |>
        str_to_lower() |>
        str_extract_all("\\w|\\d", simplify = TRUE) |>
        paste(collapse = "")

      pcs <- postal_codes$postal_code == postal_c

      if (sum(pcs) > 0) {
        loc_DAUID(postal_codes$DAUID[pcs])

        loc_name(postal_codes$postal_code[pcs] |>
                   str_to_upper() |>
                   (\(x) paste(substr(x, 1, 3), substr(x, 4, 6)))())

      } else {
        showNotification(
          paste0("No postal code found for `", input$address_searched, "`"),
          type = "error")
      }

    }) |> bindEvent(input$search_button)

    # Map click
    observe({

      loc_DAUID(input[[ns_id_map_click]]$object$ID)

      lon <- input[[ns_id_map_click]]$coordinate[[1]]
      lat <- input[[ns_id_map_click]]$coordinate[[2]]

      name <- tmaptools::rev_geocode_OSM(x = lon, y = lat)[[1]]
      town_city_county <- get_pe_loc(name)

      loc_name(paste0(name$house_number, " ", name$road, ", ",
                           town_city_county)) # One or the other

    }) |> bindEvent(input[[ns_id_map_click]])

    # Title card map click
    observe({
      loc_DAUID(input[["title_card_map_click"]]$object$ID)

      lon <- input[["title_card_map_click"]]$coordinate[[1]]
      lat <- input[["title_card_map_click"]]$coordinate[[2]]

      name <- tmaptools::rev_geocode_OSM(x = lon, y = lat)[[1]]
      town_city_county <- get_pe_loc(name)

      loc_name(paste0(name$house_number, " ", name$road, ", ",
                           town_city_county)) # One or the other

    }) |> bindEvent(input[["title_card_map_click"]])


    ## Main map updates and JS -------------------------------------------------

    widgets_name <- c("back_to_map", "grid_elements", "sidebar_widgets")

    # Show widgets when the location name updates
    observe({
      lapply(widgets_name, shinyjs::hide, anim = TRUE, animType = "fade", 
             time = 0.5)
      lapply(widgets_name, shinyjs::show, anim = TRUE, animType = "fade", 
             time = 0.5)
      shinyjs::hide("mapdeck_div", anim = TRUE, animType = "fade", time = 0.5)
    }) |> bindEvent(loc_name(), ignoreInit = TRUE)

    # Hide widgets and go back to map when the button is clicked
    observe({
      lapply(widgets_name, shinyjs::hide, anim = TRUE, animType = "fade", 
             time = 0.5)
      shinyjs::show("mapdeck_div", anim = TRUE, animType = "fade", time = 0.5)
    }) |> bindEvent(input$back_to_map)


    # Island or region comparison ----------------------------------------------

    # Reactive to toggle on or off the presence of the island_region widget
    loc_on_island <- reactive({
      !is.na(select_id()) &&
        data()$CSDUID[data()$ID == select_id()] %in% island_CSDUID
      })

    # Should we show the widget, or not? Only if select_id() is on island
    observe(toggle(id = paste0(ns_id, "-comparison_scale"),
                   condition = loc_on_island()))

    # Update dropdown language
    comparison_scale <- select_var_server(
      id = ns_id,
      r = r,
      select_var_id = "comparison_scale",
      var_list = reactive(list("Island" = "island", "Region" = "region")))

    # When select_id changes, check island/region again
    island_or_region <- reactive(if (!loc_on_island()) "region" else
      comparison_scale())
    

    ## Title card --------------------------------------------------------------

    # Draw title card map
    output$title_card_map <- renderRdeck({
      rdeck(map_style = map_base_style,
            initial_view_state = view_state(center = map_loc, zoom = 14))
    })

    # Make sure the map draws in the background, so it can respond to select_id
    outputOptions(output, "title_card_map", suspendWhenHidden = FALSE)

    # Update map on selection
    observe({
      # Get zoom and center
      zoom <- map_zoom_levels[which(df() == names(map_zoom_levels))] + 1
      if (zoom == 1) zoom <- 10
      ct <- if (is.na(select_id())) c(0, 0) else
        data()$centroid[data()$ID == select_id()][[1]]

      # Update map
      rdeck_proxy(id = "title_card_map",
                  initial_view_state = view_state(center = ct, zoom = zoom)) |>
        add_mvt_layer(id = "location",
                      data = mvt_url(paste0("sus-mcgill.", df(), "_empty")),
                      pickable = TRUE,
                      auto_highlight = TRUE,
                      highlight_color = "#AAB6CF60",
                      get_fill_color = scale_fill_pe(select_id()),
                      get_line_width = 0) |>
        add_mvt_layer(id = "DA_empty",
                      data = mvt_url(paste0("sus-mcgill.DA_empty")),
                      pickable = TRUE,
                      auto_highlight = TRUE,
                      highlight_color = "#AAB6CF60",
                      get_fill_color = scale_fill_pe(select_id()),
                      get_line_width = 0)
      })

    # Title card title
    output$title_card_title <- renderText({
      if (df() == "borough") {
        HTML("<h2>",
             paste0(borough[borough$ID == select_id(),]$name,
                    "<i style = 'color: var(--c-h2); ",
                    "font-family: var(--ff-h2); ",
                    "font-size: 2.5rem; margin-bottom: 0.75em; ",
                    "display:inline;'>",
                    "&nbsp;&nbsp;&nbsp;(",
                    borough[borough$ID == select_id(),]$name_2,
                    ")"),
             "</i></h2>")
      } else HTML("<h2 style = 'display:inline;'>",
                  paste0(sus_translate(r = r, "The area around "), loc_name(),
                         "<i style = 'color: var(--c-h2); ",
                         "font-family: var(--ff-h2); ",
                         "font-size: 2.5rem; margin-bottom: 0.75em; ",
                         "display:inline;'>",
                         "&nbsp;  (",
                         sus_translate(r = r, get_zoom_name(df())), " ", select_id(), 
                         ")"),
                  "</i></h2>")
    })
    
    title_card_to_grid <- reactive(get_title_card(r = r,
      df(), select_id(), island_or_region())) |> 
      bindCache(df(), select_id(), island_or_region(), r$lang)

    # Title card contents
    output$title_card <- renderUI({

      lapply(seq_along(title_card_to_grid()), \(x) {
        output[[paste0("ind_", x, "_plot")]] <- renderPlot({
          title_card_to_grid()[[x]][["graph"]]
          })
        })

      lapply(seq_along(title_card_to_grid()), \(x) {
        tagList(
          fluidRow(
            column(width = 2, HTML(paste0(
              "<p style = 'margin:auto; text-align:center;",
              "font-size: medium; font-weight:bold;'>",
              sus_translate(r = r, title_card_to_grid()[[x]][["row_title"]]) |>
                str_to_upper(), "</p>"))),
            column(width = 2, HTML(paste0(
              "<p style = 'margin:auto; text-align:center;'>",
              title_card_to_grid()[[x]][["percentile"]] |>
                str_to_upper(), "</p>"))),
            column(width = 2, plotOutput(eval(parse(
              text = paste0("NS(id, 'ind_", x, "_plot')"))), height = 25)),
            column(width = 6, HTML(paste0(
              "<p style = 'color: #999999; font-size:small'>",
              paste0(title_card_to_grid()[[x]][["text"]],
                     title_card_to_grid()[[x]][["link"]]),
              "</p>")))
            ),
          br()
          )
        })
    })


    ## Place explorer data -----------------------------------------------------

    output$themes_grid <- renderUI({

      # Prepare themes and text
      themes <- get_pe_themes(df(), select_id(), island_or_region())
      text_ior <- sus_translate(r = r, paste0("the ", island_or_region()))
      stand_def <- c(
        "Extreme outlier" = sus_translate(r = r, 
          "`Extreme outlier`: the variables rank in the top/bottom 10% of ",
          "{text_ior}."),
        "Outlier" = sus_translate(r = r, 
          "`Outlier`: the variables rank in the top/bottom 20% of {text_ior}."),
        "Typical" = sus_translate(r = r, 
          "`Typical`: the variables rank in the middle 60% of {text_ior}."))

      # The "server" of every block
      lapply(themes, \(x) {

        block_id <- paste0("theme_", x[1], "_block")

        output[[block_id]] <- renderUI({

          # Get the components of the block
          block <- get_pe_block(
            r = r,
            df = df(), 
            theme = x[1],
            select_id = select_id(), 
            island_or_region = island_or_region())
          
          text <- block[[1]]
          plots <- block[[2]]
          sentence <- block[[3]]

          # Only proceed if the block has data
          if (!is.null(block)) {
            
            # Render the text and plots
            lapply(seq_along(text$var_title), \(z) {
              
              output[[paste0("ind_", x[1], z, "_row_title")]] <-
                renderText({
                  paste(p(style = "font-size: 11px;",
                          sus_translate(r = r, text$var_title[z]),
                          icon("question"),
                          title = str_to_sentence(
                            sus_translate(r = r, text$explanation[z]))))
                })

              output[[paste0("ind_", x[1],  z, "_percentile")]] <-
                renderText(text$percentile[z])

              output[[paste0("ind_", x[1], z, "_value")]] <-
                renderText(text$value[z])

              output[[paste0("ind_", x[1], z, "_plot")]] <-
                renderPlot(plots[[z]])

            })
            
            trans_theme <- str_to_upper(sus_translate(r = r, x[1]))
            trans_standout <- str_to_lower(sus_translate(r = r, x[2]))
            trans_stand_def <- stand_def[[which(names(stand_def) == x[2])]]
            
            nb_values_to_show <- min(nrow(text), 5)
            
            block_title <- paste0(trans_theme, " (", trans_standout, ")")
            
            # Final block rendering
            tagList(
              h3(style = "text-transform:inherit;",
                 block_title,
                 title = trans_stand_def),
              if (!is.null(sentence)) p(style = "font-size: small;",
                                        sentence),
              lapply(seq_len(nb_values_to_show), \(z) {
                tagList(fluidRow(
                  
                  column(width = 4,
                         if (z == 1) h5(sus_translate(r = r, "Variable")),
                         htmlOutput(eval(parse(
                           text = paste0("NS(id, 'ind_", x[1], z,
                                         "_row_title')"))))),
                  
                  column(width = 2,
                         if (z == 1) h5(sus_translate(r = r, "Rank")),
                         htmlOutput(eval(parse(
                           text = paste0("NS(id, 'ind_", x[1], z,
                                         "_percentile')"))))),
                  
                  column(width = 2,
                         if (z == 1) h5(sus_translate(r = r, "Value")),
                         htmlOutput(eval(parse(
                           text = paste0("NS(id, 'ind_", x[1], z,
                                         "_value')"))))),
                  
                  column(width = 3,
                         if (z == 1) h5(sus_translate(r = r, "Plot")),
                         plotOutput(eval(parse(
                           text = paste0("NS(id, 'ind_", x[1], z,
                                         "_plot')"))),
                           height = 25))
                ),
                br()
                )
              })
            )
            
          } else tagList(fluidRow(h3(sus_translate(r = r, x[1]))),
                         fluidRow("No data."))
        }) |> bindCache(df(), select_id(), island_or_region(), x,
                        input$themes_checkbox, r$lang)
      })

      standout <- sapply(themes, \(x) x[2])
      which_standout <- which(standout %in% c("Extreme outlier", "Outlier"))
      themes <- sapply(themes, \(x) x[1])

      lapply(seq_along(themes), \(x) {
        tagList(
          if (x == 1 && length(which_standout) != 0) {
            tagList(h2(style = "padding: 10px; margin-bottom:0px",
                       sus_translate(r = r, "What makes this area unique?")))
          } else if ((x == 1 && length(which_standout) == 0) ||
                     (length(which_standout) != 0 &&
                      x - 1 == which_standout[length(which_standout)])) {
            tagList(h2(style = "padding: 10px; margin-bottom:0px",
                       sus_translate(r = r, 
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

    # Update grid based on checkbox
    observe({
      themes <- pe_theme_order[[df()]][[island_or_region()]]
      themes <- themes[themes$ID == select_id(), ]
      themes <- themes$theme

      to_hide <- themes[!themes %in% input$themes_checkbox]
      to_show <- themes[themes %in% input$themes_checkbox]

      lapply(to_hide, \(x) shinyjs::hide(paste0("theme_", x, "_block")))
      lapply(to_show, \(x) shinyjs::show(paste0("theme_", x, "_block")))
    }) |> bindEvent(input$themes_checkbox, ignoreInit = TRUE)

    
    ## Links to other modules --------------------------------------------------
    
    observe({
      z <- title_card_to_grid()[["total_crash_per1k"]]
      module_link(r = r, module = z$link_module,
                  select_id = select_id(),
                  var_left = z$link_var_left,
                  df = df())
    }) |> bindEvent(input$title_card_total_crash_per1k)

    observe({
      z <- title_card_to_grid()[["single_detached"]]
      module_link(r = r, module = z$link_module,
                  select_id = select_id(),
                  var_left = z$link_var_left,
                  df = df())
    }) |> bindEvent(input$title_card_single_detached)

    observe({
      z <- title_card_to_grid()[["green_space_ndvi"]]
      module_link(r = r, module = z$link_module,
                  select_id = select_id(),
                  var_left = z$link_var_left,
                  df = df())
    }) |> bindEvent(input$title_card_green_space_ndvi)

    observe({
      z <- title_card_to_grid()[["canale_index"]]
      module_link(r = r, module = z$link_module,
                  select_id = select_id(),
                  var_left = z$link_var_left,
                  df = df())
    }) |> bindEvent(input$title_card_canale_index)
  })
}

