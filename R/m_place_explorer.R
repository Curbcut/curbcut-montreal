### PLACE EXPLORER MODULE ######################################################

# UI ----------------------------------------------------------------------

place_explorer_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      ## STYLE ------------------------------------------------------------
      inlineCSS("#deckgl-overlay { z-index:4; }"),
      inlineCSS(list(.big_map = "width: 100%; height: 100vh;")),
      inlineCSS(list(.banner_map = "width: 100%; height: 200px;")),
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
      
      
      ## SEARCH PANEL -----------------------------------------------------
      absolutePanel(
        # id = NS(id, "search_bar"),
        style = "z-index:1000; padding:20px;",
        right = 20, top = 20,
        class = "panel panel-default",
        strong("Enter a postal code, or click on the map"),
        textInput(inputId = NS(id, "adress_searched"), label = NULL,
                  placeholder = "H3A 2T5"),
        actionButton(inputId = NS(id, "search_button"), 
                     label = "Search"),
        hidden(actionLink(inputId = NS(id, "comeback_map"),
                          label = HTML("<br>Go back to full-page map")))),
      
      
      ## MAIN MAP ---------------------------------------------------------
      div(id = NS(id, "mapdeck_div"), 
          class = "big_map", 
          mapdeckOutput(NS(id, "map"), height = "100%")),
      
      
      ## EXPLORER ONCE A LOCATION IS SELECTED -----------------------------+
      sidebar_UI2(NS(id, "place_explorer"),
                               # hidden(div(id = "sidebar_widgets",
                               
                               # fluidRow(
                                 # Checkboxes of each theme
                               susSidebarWidgets(
                                 pickerInput(
                                   inputId = NS(id, "themes_checkbox"),
                                   label = "Select theme(s):",
                                   choices = unique(variables$theme),
                                   selected = unique(variables$theme),
                                   options = list(
                                     `selected-text-format` = "count > 4"), 
                                   multiple = TRUE
                                 )),
                               
                               # Retrieve the scale the user is interested in
                               susSidebarWidgets(
                                 # fluidRow(
                                   HTML('<label class = "control-label">Select scale:</label>'),
                                   mapdeckOutput(NS(id, "scalemap"), height = 150),
                                   sliderTextInput(
                                     inputId = NS(id, "slider"), 
                                     label = NULL, 
                                     choices = get_zoom_label(map_zoom_levels[1:3]), 
                                     selected = get_zoom_label(map_zoom_levels[1:3])[3],
                                     hide_min_max = TRUE, 
                                     force_edges = TRUE)
                                 ),
                               
                               # Island only comparison, or region-wide
                               susSidebarWidgets(
                                 # fluidRow(
                                   HTML(paste0('<label id = ', NS(id, "comparison_scale_label"),
                                               ' class = "control-label">',
                                               sus_translate('Choose comparison scale:'), '</label>')),
                                   mapdeckOutput(NS(id, "island_region"), height = 150),
                                   htmlOutput(outputId = NS(id, "actual_comparison_scale"))
                                 ),
                             ),#),
      
      # Main panel as a uiOutput. The amount of themes displayed is reactive
      hidden(uiOutput(NS(id, "gridelements")))
    )))
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values initialization
    location <- reactiveVal()
    location_name <- reactiveVal()
    island_comparison <- reactiveVal("island")
    
    # Sidebar
    sidebar_server(id = "place_explorer", x = "place_explorer")
    
    
    ## MAIN MAP ----------------------------------------------------------
    output$map <- renderMapdeck(mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location) |> 
        # There must to be a base layer to enable lat-lon retrieval on click
        add_polygon(data = borough, 
                    fill_colour = NULL, 
                    stroke_opacity = 1,
                    fill_opacity = 1,
                    update_view = FALSE))
    
    
    ## RETRIEVE LOCATION ------------------------------------------------
    # Get point data from a search
    observeEvent(input$search_button, {
      
      postal_c <- str_to_lower(input$adress_searched) |> 
        str_extract_all("\\w|\\d", simplify = TRUE) |> 
        paste(collapse = "")
      
      if (postal_c %in% postal_codes$postal_code) {
        location(postal_codes[postal_codes$postal_code == postal_c, ])
        location_name(location()$postal_code |> 
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
    observeEvent(input$map_polygon_click, {
      lst <- fromJSON(input$map_polygon_click)
      
      location(st_point(c(lst$lon, lst$lat)) |> 
                 st_sfc(crs = 4326) |> 
                 as_tibble() |> 
                 st_as_sf())
      
      name <- tmaptools::rev_geocode_OSM(location())[[1]]
      
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
    observeEvent(location(), {
      mapdeck_update(map_id = NS(id, "map")) |>
        add_scatterplot(data = location(), radius = 20,
                        fill_colour = "#2A5A5BEE")
      
      hide(id = "gridelements", anim = TRUE, animType = "fade", time = 1)
      # hide(id = "sidebar_widgets", anim = TRUE, animType = "fade", time = 1)
      hide(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
      
      show(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
      show(id = "gridelements", anim = TRUE, animType = "fade", time = 1)
      show(id = "sidebar_widgets", anim = TRUE, animType = "fade", time = 1)
      removeCssClass(id = "mapdeck_div", class = "big_map")
      addCssClass(id = "mapdeck_div", class = "banner_map") 
    }, ignoreInit = TRUE)
    
    # Hook up the go back to map
    observeEvent(input$comeback_map, {
      mapdeck_update(map_id = NS(id, "map")) |>
        clear_scatterplot()
      
      removeCssClass(id = "mapdeck_div", class = "banner_map")
      addCssClass(id = "mapdeck_div", class = "big_map") 
      
      hide(id = "gridelements", anim = TRUE, animType = "fade", time = 1)
      # hide(id = "sidebar_widgets", anim = TRUE, animType = "fade", time = 1)
      hide(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
    })
    
    
    
    ## RETRIEVE df AND ROW ID ------------------------------------------
    # Reactive map depending on location
    output$scalemap <- renderMapdeck({
      update_scale_map(id_map = NS(id, "scalemap"), location = location(),
                       init = TRUE)
    })
    observeEvent(location(), {
      update_scale_map(id_map = NS(id, "scalemap"), location = location(),
                       init = FALSE)
    })
    
    # Change df on scalemap click
    observeEvent(input$scalemap_polygon_click, {
      select_df <- fromJSON(input$scalemap_polygon_click)$object$properties$id
      # A click on the map triggers a change in the slider, domino to `df`
      updateSliderTextInput(session = session, 
                            inputId = "slider",
                            selected = get_zoom_name(select_df))
    })
    df <- reactive(get_zoom_code(input$slider))
    
    # Depending on `df`, retrieve the ID.
    select_id <- reactive({
      if (!is.null(location())) {
        st_intersection(get(df()), location())$ID
      } else NULL
    })
    
    
    ## ISLAND OR REGION COMPARISON -------------------------------------
    output$island_region <- renderMapdeck(island_region_map(location()))
    
    location_on_island <- reactive({
      if (!is.null(location())) {
        filter(get(df()), ID == select_id())$CSDUID %in% island_CSDUID
      }})
    
    # Should we show the map, or not? Only if location() is on island
    observe({
      toggle(id = "island_region", condition = location_on_island())
      toggle(id = "comparison_scale_label", condition = location_on_island())
      toggle(id = "actual_comparison_scale", condition = location_on_island())
    })
    
    # island or region reactive.
    observeEvent(input$island_region_polygon_click, {
      island_comparison({
        fromJSON(input$island_region_polygon_click)$object$properties$id})
    })
    
    # Let the user know what is the actual scale
    output$actual_comparison_scale <- renderText({
      scale <- str_to_sentence(sus_translate(island_comparison()))
      sus_translate("Actual scale: {scale}")
    })
    
    
    ## PLACE EXPLORER DATA ----------------------------------------------
    output$gridelements <- renderUI({
      
      # TITLE CARD
      output$title_card <- renderUI({
        
        output$title <- renderText({
          HTML("<h2>", 
               if (df() == "borough") {
                 borough[borough$ID == select_id(),]$name
               } else location_name(), 
               "</h2>")
        })
        
        output$list <- renderUI({
          
          to_grid <- title_card_indicators_fun(
            df(), select_id(),
            island_only_comparison = island_comparison())
          
          map(names(to_grid), ~{
            output[[paste0("ind_", .x, "_row_title")]] <- renderText({
              to_grid[[.x]][["row_title"]] |> 
                str_to_upper()
            })
            output[[paste0("ind_", .x, "_plot")]] <- renderPlot({
              to_grid[[.x]][["graph"]]
            })
            output[[paste0("ind_", .x, "_text")]] <- renderText({
              to_grid[[.x]][["text"]]
            })
          })
          
          map(names(to_grid), ~{
            tagList(
              fluidRow(
                column(width = 3, 
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", .x, "_row_title')"))),
                         style = paste0("margin:auto; text-align:center; ",
                                        "font-size: medium; font-weight:bold;"))),
                column(width = 2,
                       plotOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", .x, "_plot')"))),
                         height = 25)),
                column(width = 7,
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", .x, "_text')"))),
                         style = "color: #999999"))
                
              ),
              br()
            )
          })
          
        })
        
        # If the renderUI is reran, it reruns a mapdeck with the same name,
        # which makes it lag. I mark every iteration with the time, so a new
        # mapdeck gets created every time.
        time <- Sys.time() |> str_replace_all(" |:|-", "_")
        
        output[[paste("title_card_plot", time)]] <- renderMapdeck({
          data <- get(df()) |>
            filter(ID == select_id()) |>
            select(-everything()) |>
            mutate(tooltip = df())
          
          mapdeck(
            style = map_style,
            token = map_token,
            zoom = map_zoom,
            location = map_location) |>
            add_polygon(data = data,
                        tooltip = "tooltip",
                        highlight_colour = "#FFFFFF80",
                        fill_colour = "#BAE4B3BB",
                        stroke_colour = "#FFFFFF",
                        stroke_width = 10,
                        auto_highlight = TRUE,
                        update_view = TRUE)
        })
        
        tagList(
          column(8, fluidRow(htmlOutput(NS(id, "title")),
                             uiOutput(NS(id, "list")))),
          column(4, mapdeckOutput(NS(id, paste("title_card_plot", time)))))
      })
      
      
      themes <-
        pe_theme_order[[df()]] |>
        filter(ID == select_id(), theme %in% input$themes_checkbox) |>
        arrange(theme_order) |>
        pull(theme)
      
      # The "server" of every block
      delay(1000, {
      walk(themes, function(theme) {
        block <- paste0("theme_", theme, "_block")
        
        output[[block]] <- renderUI({
          
          to_grid <- place_explorer_block_text(
            df = df(), 
            theme = theme,
            select_id = select_id(),
            island_only_comparison = island_comparison())
          
          plots <- place_explorer_block_plot(
            df = df(), 
            theme = theme,
            select_id = select_id(),
            island_only_comparison = island_comparison()
          )
          
          map(1:(nrow(to_grid)), ~{
            output[[paste0("ind_", theme, .x, "_row_title")]] <- renderText({
              
              paste(to_grid[.x, ][["var_title"]],
                    icon("question", 
                         title = str_to_sentence(to_grid[.x, ][["explanation"]])))
            })
            output[[paste0("ind_", theme,  .x, "_percentile")]] <- renderText({
              to_grid[.x, ][["percentile"]]
            })
            output[[paste0("ind_", theme, .x, "_value")]] <- renderText({
              to_grid[.x, ][["value"]]
            })
            output[[paste0("ind_", theme, .x, "_plot")]] <- renderPlot({
              plots[[.x]]
            })
          })
            
          map(1:(nrow(to_grid)), ~{
            tagList(
              fluidRow(
                column(width = 4, 
                       if (.x == 1) h5(sus_translate("Variable")),
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", theme, .x, "_row_title')"))))),
                column(width = 2,
                       if (.x == 1) h5(sus_translate("Percentile")),
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", theme, .x, "_percentile')"))))),
                column(width = 2,
                       if (.x == 1) h5(sus_translate("Value")),
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", theme, .x, "_value')"))))),
                column(width = 3,
                       if (.x == 1) h5(sus_translate("Plot")),
                       plotOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", theme, .x, "_plot')"))),
                         height = 25))
              ),
              br()
            )
          })
          
        })
        
      })

      # Prepare the UI of each block
      walk(themes, ~{
        output_name <- paste0("theme_", .x)
        # Render UI of each grid block
        output[[output_name]] <- renderUI({
          tagList(
            h3(sus_translate(.x)),
            uiOutput(eval(parse(text = paste0("NS(id, 'theme_", .x, "_block')"))))
          )
          
        })
      })
      })
      
      # Change class of a ui depending on its location
      
      # Prepare the general UI UI of blocks
      
      fluidPage(
        div(style = paste0("margin-top:225px; overflow-x: hidden; ",
                           "overflow-y: auto;  height: calc(100vh - 310px);",
                           "margin-left:310px; background-color:#ffffff;"),
            uiOutput(NS(id, "title_card"), 
                     style = paste0("padding: 5px;",
                                    "font-size: 11px;",
                                    "max-width: 1200px; margin:auto;",
                                    "padding:30px;")),
            imap(themes, ~{
              tagList(uiOutput(
                outputId = eval(parse(text = paste0("NS(id, 'theme_", .x, "')"))),
                style = paste0("padding: 20px; margin: 10px; font-size: 11px;",
                               "display: inline-grid; ",
                               "overflow-y: auto; overflow-x: hidden;",
                               "width: 48%"), 
                class = "panel panel-default "))
            })
        )
      )
      
    })
    
  })
}
