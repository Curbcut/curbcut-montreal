### STORIES MODULE ##############################################################

# UI ----------------------------------------------------------------------

stories_UI <- function(id) {
  fillPage(fillRow(
    fillCol(
      
      # Sidebar
      sidebar_UI(
        NS(id, "sidebar"),
        hr(id = NS(id, "hr")),
        actionLink(NS(id, "back"), i18n$t("Back to the map")))),
    
    fillCol(
      
      # Map
      div(class = "mapdeck_div", mapdeckOutput(NS(id, "map"), height = "100%")),
      
      shinyjs::hidden(htmlOutput(
        NS(id, "stories"),
        style = paste0("position:absolute; margin: 40px; ",
                       "max-width: 1000px; z-index:499")))),
    
    flex = c(1, 5)))
}


# Server ------------------------------------------------------------------

stories_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initial reactives
    selection <- reactiveVal(NA)
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
      location = map_location)})
    
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
        v3 <- paste0(
          purrr::map(data()$buffer, st_bbox) %>% purrr::map(., as.vector))
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
        selection(NA)
      } else if (!is.na(selection()) && 
                 click == selection()) {
        selection(NA)
      } else selection(click)
    })
    
    # Render the story in question, now only in english (_en)
    output$stories <- renderUI({
      
      if (!is.na(selection())) {
        
      rmd_name <- stories[stories$ID == selection(),]$rmd
      bandeau_name <- stories[stories$ID == selection(),]$img
      
      HTML('<div class = "main_panel_text_popup">',
           # Adding bandeau img after the first div (title)
           str_replace(includeHTML(paste0("www/stories/", rmd_name, "_en.html")),
                       "</div>", paste0("</div><img src =", "stories/bandeau_img/",
                       bandeau_name,"><br><br>")),
           '</div>')
      }
      
    })
    
    # Anytime to "Go back to map" button is clicked, poly_selected goes
    # NA causing the div to disappear
    observeEvent(input$back, {
      selection(NA)
    })
    
    observeEvent(selection(), {
      shinyjs::toggle("hr", condition = !is.na(selection()))
      shinyjs::toggle("back", condition = !is.na(selection()))
      shinyjs::toggle("stories", condition = !is.na(selection()))
    })
  
    # If there's an action with the map, the rmd goes away (Ultimately, any click on the map
    # should trigger these)
    observeEvent(input$map_view_change, {
      shinyjs::hide(id = "stories")
      selection(NA)
    })
    
  })
}
