### STORIES MODULE ##############################################################

# UI ----------------------------------------------------------------------

stories_UI <- function(id) {
  tagList(
    
    # Sidebar
    sidebar_UI(
      NS(id, "sidebar"),
      hr(id = NS(id, "hr")),
      actionLink(NS(id, "back"), sus_translate("Back to the map"))),
    
    # Map
    div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
    
    # Stories
    hidden(htmlOutput(
      NS(id, "stories"),
      style = paste0("position:absolute; margin: 40px; ",
                     "max-width: 1200px; z-index:499")))
    
  )
}


# Server ------------------------------------------------------------------

stories_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns_id <- "stories"
    
    # Initial reactives
    select_id <- reactiveVal(NA)
    zoom <- reactiveVal(map_zoom * -550 + 8000)
    
    # Sidebar
    sidebar_server(
      id = "sidebar", 
      x = "stories")

    # Map
    output$map <- renderMapdeck({mapdeck(
      style = map_style, 
      token = map_token, 
      zoom = map_zoom, 
      location = map_location) |> 
        # Initiate a basemap so that a click outside the popup 
        # have the power to deselect it.
        add_polygon(data = borough, 
                    fill_colour = NULL, 
                    stroke_opacity = 1,
                    fill_opacity = 1,
                    update_view = FALSE,
                    layer_id = "basemap")})
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      zoom(case_when(input$map_view_change$zoom > 13 ~ 600,
                     TRUE ~ input$map_view_change$zoom * -550 + 8000))
    })
    
    
    # Update buffer to change the map when zoom is different
    data <- reactive({
        stories %>%
        mutate(buffer = st_buffer(geometry, zoom())) %>%
        st_set_geometry("buffer")
    })
    
    observeEvent({
      data()
      zoom()}, {

        v1 <- 1:nrow(data())
        v2 <- paste0("stories/round_img/", data()$img[v1])
        v3 <- paste0(map(data()$buffer, st_bbox) %>% map(as.vector))
        v4 <- paste0("image", v1)
        
        all_add_bitmap <- paste0('add_bitmap("', v2, '", bounds = ', v3, ', ',
                                 'layer_id ="',v4 ,'", update_view = FALSE) %>% ')
        
        all_add_bitmap[length(all_add_bitmap)] <-
          str_remove_all(all_add_bitmap[length(all_add_bitmap)], "%>%")
        
        updated_map <-
          paste0('mapdeck_update(map_id = NS(id, "map")) %>%',
                 'add_polygon(data(),',
                      'id = "ID", fill_opacity = 0, layer_id = "clickable",',
                      'stroke_opacity = 0, update_view = FALSE) %>% ',
                 parse(text = all_add_bitmap))
        
        eval(parse(text = updated_map))
        
  })
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      click <- fromJSON(input$map_polygon_click)$object$properties$id
      if (is.null(click)) {
        select_id(NA)
        hide(id = "stories")
      } else if (!is.na(select_id()) && 
                 click == select_id()) {
        select_id(NA)
        hide(id = "stories")
      } else select_id(click)
    })
    
    # Render the story in question, now only in english (_en)
    output$stories <- renderUI({
      
      if (!is.na(select_id())) {
        
        rmd_name <- stories[stories$ID == select_id(),]$name
        bandeau_name <- stories[stories$ID == select_id(),]$img
        
        HTML('<div class = "main_panel_text_popup">',
             # Adding bandeau img after the first div (title)
             str_replace(
               includeHTML(paste0("www/stories/", rmd_name, "_en.html")),
               "</div>", paste0("</div><img src =", "stories/bandeau_img/",
                                bandeau_name,"><br><br>")),
             '</div>')
      }
      
    })
    
    # Anytime to "Go back to map" button is clicked, poly_selected goes
    # NA causing the div to disappear
    observeEvent(input$back, {
      select_id(NA)
    })
    
    observeEvent(select_id(), {
      toggle("hr", condition = !is.na(select_id()))
      toggle("back", condition = !is.na(select_id()))
      toggle("stories", condition = !is.na(select_id()))
    })
  
    # If there's an action with the map, the rmd goes away (Ultimately, any click on the map
    # should trigger these)
    observeEvent(input$map_view_change, {
      hide(id = "stories")
      select_id(NA)
    })
    
    # Bookmarking
    bookmark_server(
      id = ns_id,
      map_view_change = reactive(input$map_view_change),
      select_id = select_id,
      map_id = NS(id, "map"),
    )
    
    # Update click_id() on bookmark
    observeEvent(sus_bookmark$active, {
      # Delay of 500 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      if (isTRUE(sus_bookmark$active)) {
        delay(2000, {
          if (!is.null(sus_bookmark$select_id)) {
            if (sus_bookmark$select_id != "NA") select_id(sus_bookmark$select_id)
          }
        })
      }
      
      # So that bookmarking gets triggered only ONCE
      delay(1500, {sus_bookmark$active <- FALSE})      
    }, priority = -2)
    
    # Update click_id() on modulke link
    observeEvent(sus_link$activity, {
      # Delay of 500 milliseconds more than the zoom update from bookmark.
      # The map/df/data needs to be updated before we select an ID.
      delay(2000, {
        if (!is.null(sus_link$select_id)) select_id(sus_link$select_id)
      })
    }, priority = -2)
    
  })
}
