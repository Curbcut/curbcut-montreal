### PLACE EXPLORER MODULE ######################################################

# UI ----------------------------------------------------------------------

place_explorer_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      ## STYLE ------------------------------------------------------------
      inlineCSS("#deckgl-overlay { z-index:4; }"),
      inlineCSS(list(.big_map = "width: 100%; height: 100vh;")),
      inlineCSS(list(.banner_map = "width: 100%; height: 125px;")),
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
      
      ## MAIN MAP ---------------------------------------------------------
      div(id = NS(id, "mapdeck_div"), 
          class = "big_map", 
          mapdeckOutput(NS(id, "map"), height = "100%")),
      
      
      ## SIDEBAR
      sidebar_UI(NS(id, "place_explorer"),
                 
                 hidden(actionLink(inputId = NS(id, "comeback_map"),
                                   label = sus_translate("Go back to map"))),
                 
                 susSidebarWidgets(
                   # Search box
                   strong(sus_translate("Enter a postal code, ",
                                        "or click on the map")),
                   textInput(inputId = NS(id, "adress_searched"), label = NULL,
                             placeholder = "H3A 2T5"),
                   actionButton(inputId = NS(id, "search_button"), 
                                label = "Search"),
                   
                   br(), br(),
                   
                   hidden(div(id = NS(id, "sidebar_widgets"),
                              
                              # Checkboxes of each theme
                              pickerInput(
                                inputId = NS(id, "themes_checkbox"),
                                label = "Select theme(s):",
                                choices = unique(variables$theme),
                                selected = unique(variables$theme),
                                options = list(
                                  `selected-text-format` = "count > 4"), 
                                multiple = TRUE
                              ),
                              
                              br(),
                              
                              # Retrieve the scale the user is interested in
                              HTML(paste0('<label id = "', NS(id, "scalemap_label"),
                                          '" class = "control-label">',
                                          sus_translate('Select scale'), ':</label>')),
                              mapdeckOutput(NS(id, "scalemap"), height = 150),
                              sliderTextInput(
                                inputId = NS(id, "slider"), 
                                label = NULL, 
                                choices = get_zoom_label(map_zoom_levels[1:3]), 
                                selected = get_zoom_label(map_zoom_levels[1:3])[3],
                                hide_min_max = TRUE, 
                                force_edges = TRUE),
                              
                              br(),
                              
                              # Island only comparison, or region-wide
                              HTML(paste0('<label id = "', NS(id, "comparison_label"),
                                          '" class = "control-label">',
                                          sus_translate('Choose comparison scale'), ':</label>')),
                              mapdeckOutput(NS(id, "island_region"), height = 150),
                              htmlOutput(outputId = NS(id, "actual_comparison_scale"), 
                                         style = "display:none;")
                              
                   )))),
      
      # Main panel as a uiOutput. The amount of themes displayed is reactive
      fluidPage(
        hidden(div(id = NS(id, "grid_elements"), 
                   style = paste0("margin-top:150px; overflow-x: hidden; ",
                                  "overflow-y: auto;  height: calc(100vh - 235px);",
                                  "margin-left:310px; background-color:#ffffff;",
                                  "padding:25px;"),
                   fluidRow(
                     style = paste0("padding: 5px;",
                                    "font-size: 11px;",
                                    "max-width: 1200px; margin:auto;",
                                    "padding:30px;"),
                     column(8, htmlOutput(NS(id, "title_card_title")),
                            uiOutput(NS(id, "title_card"))),
                     column(4, mapdeckOutput(NS(id, "title_card_map")))),
                   
                   fluidRow(uiOutput(NS(id, "themes_grid")))))),
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
    widgets_name <- c("gridelements", "comeback_map",
                      "grid_elements", "sidebar_widgets")
    
    observeEvent(location(), {
      mapdeck_update(map_id = NS(id, "map")) |>
        add_scatterplot(data = location(), radius = 20,
                        fill_colour = "#2A5A5BEE")
      
      walk(widgets_name, ~hide(.x, anim = TRUE, animType = "fade", time = 1))
      walk(widgets_name, ~show(.x, anim = TRUE, animType = "fade", time = 1))
      removeCssClass(id = "mapdeck_div", class = "big_map")
      addCssClass(id = "mapdeck_div", class = "banner_map") 
    }, ignoreInit = TRUE)
    
    # Hook up the go back to map
    observeEvent(input$comeback_map, {
      mapdeck_update(map_id = NS(id, "map")) |>
        clear_scatterplot()
      
      removeCssClass(id = "mapdeck_div", class = "banner_map")
      addCssClass(id = "mapdeck_div", class = "big_map") 
      walk(widgets_name, ~hide(.x, anim = TRUE, animType = "fade", time = 1))
      
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
    output$island_region <- renderMapdeck({if (!is.null(location())) 
      island_region_map(location())})
    
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
      if (!is.null(location())) {
        scale <- str_to_sentence(sus_translate(island_comparison()))
        sus_translate("Actual scale: {scale}")}
    })
    
    
    ## TITLE CARD -------------------------------------------------------
    shinyjs::delay(1, shinyjs::show("grid_elements"))
    shinyjs::delay(500, shinyjs::hide("grid_elements"))
    
    output$title_card_map <- renderMapdeck({
      mapdeck(
        style = map_style, 
        token = map_token, 
        zoom = map_zoom, 
        location = map_location)
    })
    
    observeEvent({df()
      location()
      select_id()}, {
        
        if (!is.null(df()) && !is.null(select_id()) && !is.null(location())) {
          data <- get(df()) |>
            filter(ID == select_id()) |>
            select(-everything()) |>
            mutate(tooltip = df())
          
          mapdeck_update(map_id = NS(id, "title_card_map")) |>
            add_polygon(data = data,
                        tooltip = "tooltip",
                        highlight_colour = "#FFFFFF80",
                        fill_colour = "#BAE4B3BB",
                        stroke_colour = "#FFFFFF",
                        stroke_width = 5,
                        auto_highlight = TRUE,
                        update_view = TRUE)
        }
      })
    
    output$title_card_title <- renderText({
      if (!is.null(df()) && !is.null(select_id()) && !is.null(location())) {
        HTML("<h2>",
             if (df() == "borough") {
               borough[borough$ID == select_id(),]$name
             } else location_name(),
             "</h2>")
      } else HTML("<h2>Your selected location</h2>")
    })
    
    output$title_card <- renderUI({
      
      output$list <- renderUI({
        if (!is.null(df()) && !is.null(select_id()) && !is.null(location())) {
          
          to_grid <- get_title_card(
            df(), select_id(),
            island_or_region = island_comparison())
          
          map(seq_along(to_grid), ~{
            output[[paste0("ind_", .x, "_row_title")]] <- renderText({
              to_grid[[.x]][["row_title"]] |>
                str_to_upper()
            })
            output[[paste0("ind_", .x, "_percentile")]] <- renderText({
              to_grid[[.x]][["percentile"]] |>
                str_to_upper()
            })
            output[[paste0("ind_", .x, "_plot")]] <- renderPlot({
              to_grid[[.x]][["graph"]]
            })
            output[[paste0("ind_", .x, "_text")]] <- renderText({
              to_grid[[.x]][["text"]]
            })
          })
          
          map(seq_along(to_grid), ~{
            tagList(
              fluidRow(
                column(width = 2,
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", .x, "_row_title')"))),
                         style = paste0("margin:auto; text-align:center; ",
                                        "font-size: medium; font-weight:bold;"))),
                column(width = 2,
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", .x, "_percentile')"))),
                         style = paste0("margin:auto; text-align:center;"))),
                column(width = 2,
                       plotOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", .x, "_plot')"))),
                         height = 25)),
                column(width = 6,
                       htmlOutput(eval(parse(
                         text = paste0("NS(id, 'ind_", .x, "_text')"))),
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
      if (!is.null(df()) && !is.null(select_id()) && !is.null(location())) {
        
        themes <-
          pe_theme_order[[df()]] |>
          filter(ID == select_id(), theme %in% input$themes_checkbox) |>
          filter(group == !!island_comparison()) |> 
          arrange(theme_order) |>
          pull(theme)
        
        # The "server" of every block
        iwalk(themes, function(theme, ite) {
          delay(ite*100, {
            block <- paste0("theme_", theme, "_block")
            
            output[[block]] <- renderUI({
              
              to_grid <- place_explorer_block_text(
                df = df(), 
                theme = theme,
                select_id = select_id(),
                island_or_region = island_comparison())
              
              plots <- place_explorer_block_plot(
                df = df(), 
                theme = theme,
                select_id = select_id(),
                island_or_region = island_comparison()
              )
              
              if (nrow(to_grid) > 0)
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
              
              if (nrow(to_grid) > 0) {
              tagList(h3(sus_translate(theme)),
                      map(1:(nrow(to_grid)), ~{
                        tagList(
                          fluidRow(
                            column(width = 4, 
                                   if (.x == 1) h5(sus_translate("Variable")),
                                   htmlOutput(eval(parse(
                                     text = paste0("NS(id, 'ind_", theme, .x, "_row_title')"))))),
                            column(width = 2,
                                   if (.x == 1) h5(sus_translate("Rank")),
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
              )} else {
                tagList(fluidRow(h3(sus_translate(theme))), 
                        fluidRow("No data."))
              }
            })
          })
        })
        
        map(themes, ~{
          tagList(uiOutput(
            outputId = eval(parse(text = paste0("NS(id, 'theme_", .x, "_block')"))),
            style = paste0("padding: 20px; margin: 10px; font-size: 11px;",
                           "display: inline-grid; width: 48%;"), 
            class = "panel panel-default "))
        })
      }
    })
  })
}
