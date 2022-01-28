### PLACE EXPLORER MODULE ######################################################

# UI ----------------------------------------------------------------------

place_explorer_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
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
'), `data-proxy-click` = "place_explorer-search_button",
      
      # Search bar
      absolutePanel(
        id = NS(id, "search_bar"), 
        style = "z-index:500; padding: 5px; border-width: 0.5px; text-align: center;",
        class = "panel panel-default", top = 20, left = 15,
        strong("Enter a postal code, or click on the map"),
        # splitLayout(cellWidths = c("70%", "30%"),
        # Autocompletion at least for postal code would be great
        textInput(inputId = NS(id, "adress_searched"), label = NULL,
                  # placeholder = "H3A 2T5",
                  value = "H3A 2T5"),
        actionButton(inputId = NS(id, "search_button"), 
                     label = "Search"),#),
        hidden(actionLink(inputId = NS(id, "comeback_map"),
                          label = HTML("<br>Go back to the map")))),
      
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Blog post
      hidden(uiOutput(NS(id, "gridelements"))),
      # Blog post toolbox
      hidden(absolutePanel(
        id = NS(id, "toolbox"), 
        style = "z-index:500; padding: 5px; border-width: 0.5px; max-width:40vw; text-align: center;",
        class = "panel panel-default", top = 20, right = 15,
        # Retrieve the df the user is interested in
        sliderTextInput(
          inputId = NS(id, "slider"), 
          label = NULL, 
          choices = get_zoom_label(map_zoom_levels[1:3]), 
          hide_min_max = TRUE, 
          force_edges = TRUE),
        # Checkboxes of each theme
        checkboxGroupButtons(
          inputId = NS(id, "themes_checkbox"),
          label = "Select theme(s):",
          choices = unique(variables$theme),
          selected = unique(variables$theme),
          individual = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle", 
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-circle-o", 
                        style = "color: steelblue"))
        )
      )),
      
      
    )))
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load data used by this module
    if (!exists("pe_var_hierarchy")) {
      qload("data/place_explorer.qsm")
    }
    
    # Reactive values initialization
    location <- reactiveVal()
    location_name <- reactiveVal()
    
    # Map
    output$map <- renderMapdeck(mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location) |> 
        # There must to be a base layer to enable click lat-lon retrieval
        add_polygon(data = borough, 
                    fill_colour = NULL, 
                    stroke_opacity = 1,
                    fill_opacity = 1,
                    update_view = FALSE))
    
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
        
        # If it is a street address
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
    
    # Update map depending on selection
    observeEvent(location(), {
      hide(id = "gridelements", anim = TRUE, animType = "fade", time = 1)
      hide(id = "toolboc", anim = TRUE, animType = "fade", time = 1)
      show(id = "map", anim = TRUE, animType = "fade", time = 1)
      hide(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
      
      mapdeck_update(map_id = NS(id, "map")) |>
        add_scatterplot(data = location(), radius = 20,
                        fill_colour = "#2A5A5BEE")
      
      hide(id = "map", anim = TRUE, animType = "fade", time = 1)
      show(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
      show(id = "gridelements", anim = TRUE, animType = "fade", time = 1)
      show(id = "toolbox", anim = TRUE, animType = "fade", time = 1)
    }, ignoreInit = TRUE)
    
    # Hook up the go back to map
    observeEvent(input$comeback_map, {
      mapdeck_update(map_id = NS(id, "map")) |>
        clear_scatterplot()
      
      hide(id = "gridelements", anim = TRUE, animType = "fade", time = 1)
      hide(id = "toolbox", anim = TRUE, animType = "fade", time = 1)
      show(id = "map", anim = TRUE, animType = "fade", time = 1)
      hide(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
    })
    
    # Retrieve the df the user is interested in
    df <- reactive(get_zoom_code(input$slider))
    
    # Retrieve the select_id
    select_id <- reactive({
      if (!is.null(location())) {
        st_intersection(get(df()), location())$ID
      } else NULL
    })
    
    # Gird elements
    output$gridelements <- renderUI({
      
      output$title <- renderText(HTML("<h3>", location_name(), "</h3>"))
      
      themes <- 
        pe_theme_order[[df()]] |> 
        filter(ID == select_id(), theme %in% input$themes_checkbox) |> 
        arrange(theme_order) |> 
        pull(theme)
        
      # The "server" of every block
      walk(themes, ~{
        output_info_name <- paste0("theme_", .x, "_block_text")
        output_graph_name <- paste0("theme_", .x, "_block_graph")
        # Render UIs of each grid block
        output[[output_info_name]] <- renderText({
          selected_var <- input[[paste0("theme_", .x, "_block_select")]]
          place_explorer_block_text(df(), selected_var, select_id())
        })
        output[[output_graph_name]] <- renderPlot({
          selected_var <- input[[paste0("theme_", .x, "_block_select")]]
          place_explorer_block_graph(df(), selected_var, select_id())
        })
      })
      
      # observe({
      #   walk(themes, ~{
      #     input[[paste0("theme_", .x, "_link")]]
      # })
      
      # Prepare the UI of each block
      walk(themes, ~{
        output_name <- paste0("theme_", .x)
        # Render UI of each grid block
        output[[output_name]] <- renderUI({
          
          variables_arranged <- 
            pe_variable_order[[df()]] |> 
            filter(ID == select_id(), theme == .x) |> 
            arrange(variable_order) |> 
            select(var_code) |> 
            left_join(variables, by = "var_code")
          
          selection_list <- as.list(variables_arranged$var_code)
          names(selection_list) <- as.list(variables_arranged$var_title)
              tagList(
                div(id = eval(parse(text = paste0("NS(id, 'theme_", .x, "_block_title')"))),
                    fluidRow(column(width = 7, h4(i18n$t(.x))),
                             column(width = 5, align = "right", 
                                    actionLink(inputId = eval(parse(text = paste0("NS(id, 'theme_", .x, "_link')"))), 
                                               label = i18n$t("Disabled action Link"))))),
                
                div(id = eval(parse(text = paste0("NS(id, 'theme_", .x, "_block_content')"))),
                    selectInput(inputId = eval(parse(text = paste0("NS(id, 'theme_", .x, "_block_select')"))),
                                choices = selection_list,
                                label = NULL),
                    htmlOutput(eval(parse(text = paste0("NS(id, 'theme_", .x, "_block_text')")))),
                    plotOutput(eval(parse(text = paste0("NS(id, 'theme_", .x, "_block_graph')"))), height = 150))
              )
            })
        })
      

      # Change class of a ui depending on its location

      # Prepare the general UI UI of blocks
      
      fixedPage(
      inlineCSS(list(.smallblock = "width: 45%")),
      inlineCSS(list(.bigblock = "width: calc(90% + 20px)")),
        
        htmlOutput(NS(id, "title"), 
                   style = paste0("margin-top: 150px; padding: 5px; ",
                                  "font-size: 11px;")),
        imap(themes, ~{
          
          # Only first element starts with a bigblock
          block_size <- if (.y == 1) "bigblock" else "smallblock"
          
          tagList(uiOutput(
            outputId = eval(parse(text = paste0("NS(id, 'theme_", .x, "')"))),
            style = paste0("padding: 20px; margin: 10px; font-size: 11px;",
                           "height: 33vh; display: inline-grid; ",
                           "overflow-y: auto; overflow-x: hidden;"), 
            class = paste0("panel panel-default ", block_size)))
        })
      )
      
    })
    
  })
}