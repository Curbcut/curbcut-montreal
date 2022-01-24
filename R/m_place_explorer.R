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
        style = paste0("z-index:500; padding: 5px; border-width: 0px;",
                       "font-size: 11px;"),
        class = "panel panel-default", top = 20, left = 15,
        strong("Enter a postal code, or click on the map"),
        splitLayout(cellWidths = c("70%", "30%"),
                    # Autocompletion at least for postal code would be great
                    textInput(inputId = NS(id, "adress_searched"), label = NULL,
                              # placeholder = "H3A 2T5",
                              value = "H3A 2T5"),
                    actionButton(inputId = NS(id, "search_button"), 
                                 label = "Search")),
        hidden(actionLink(inputId = NS(id, "comeback_map"),
                          label = HTML("<br>Go back to the map")))),
      
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      # Blog post
      hidden(uiOutput(NS(id, "blogpost")))
      
      
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
      hide(id = "blogpost", anim = TRUE, animType = "fade", time = 1)
      show(id = "map", anim = TRUE, animType = "fade", time = 1)
      hide(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
      
      mapdeck_update(map_id = NS(id, "map")) |>
        add_scatterplot(data = location(), radius = 20,
                        fill_colour = "#2A5A5BEE")
      
      hide(id = "map", anim = TRUE, animType = "fade", time = 1)
      show(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
      show(id = "blogpost", anim = TRUE, animType = "fade", time = 1)
    }, ignoreInit = TRUE)
    
    # Hook up the go back to map
    observeEvent(input$comeback_map, {
      mapdeck_update(map_id = NS(id, "map")) |>
        clear_scatterplot()
      
      hide(id = "blogpost", anim = TRUE, animType = "fade", time = 1)
      show(id = "map", anim = TRUE, animType = "fade", time = 1)
      hide(id = "comeback_map", anim = TRUE, animType = "fade", time = 1)
    })
    
    # Block post style
    output$blogpost <- renderUI({
      output$title <- renderText(HTML("<h3>", location_name(), "</h3>"))
      output$first_element <- renderText("second")
      output$second_element <- renderText("third")
      
      fixedPage(
        verticalLayout(
        htmlOutput(NS(id, "title"), style = paste0("margin-top: 150px; padding: 5px; ",
                                                   "font-size: 11px;")),
        htmlOutput(NS(id, "first_element"), style = paste0("padding: 5px; ",
                                                           "font-size: 11px"), 
                   class = "panel panel-default"),
        htmlOutput(NS(id, "second_element"), style = paste0("padding: 5px; ",
                                                            "font-size: 11px"), 
                   class = "panel panel-default"),
        
        )
      )
      
    })
    
  })
}