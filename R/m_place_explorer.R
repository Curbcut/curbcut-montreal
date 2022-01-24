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
        # checkboxGroupInput(inputId = NS(id, "themes_checkbox"),
        #                    label = "Themes",
        #                    choices = unique(variables$theme),
        #                    selected = unique(variables$theme),
        #                    inline = TRUE)
        selectInput(
          inputId = NS(id, "themes_checkbox"),
          label = "Select theme(s):",
          choices = unique(variables$theme),
          selected = unique(variables$theme),
          multiple = TRUE,
          selectize = TRUE
        )
        )),
      
      
    )))
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
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
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      
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
    
    # Gird elements
    output$gridelements <- renderUI({
      
      output$title <- renderText(HTML("<h3>", location_name(), "</h3>"))
      
      themes <- input$themes_checkbox |> 
        str_to_lower() |> 
        str_replace(" ", "_")

      iwalk(themes, ~{
        output_name <- paste0("theme_", .x)
        output[[output_name]] <- renderText(as.character(.x))
      })
      
      fixedPage(verticalLayout(
        htmlOutput(NS(id, "title"), 
                   style = paste0("margin-top: 150px; padding: 5px; ",
                                  "font-size: 11px;")),
        imap(themes, ~{
          tagList(htmlOutput(
            outputId = eval(parse(text = paste0("NS(id, 'theme_", .x, "')"))),
            style = paste0("padding: 5px; ",
                           "font-size: 11px"), 
            class = "panel panel-default"))
        })
      ))
      
    })
    
  })
}