### STORIES MODULE ##############################################################

# UI ----------------------------------------------------------------------

stories_UI <- function(id) {
  fillPage(
    fillRow(
      fillCol(sidebar_UI(NS(id, "sidebar"),
                         hr(id = NS(id, "hr")),
                         actionLink(NS(id, "back"), i18n$t("Back to the map"))
      )),
      fillCol(
        div(class = "mapdeck_div", 
            mapdeckOutput(NS(id, "map"), height = "100%")),
        shinyjs::hidden(htmlOutput(NS(id, "stories"),
                                   style = "position:absolute; margin: 40px;
                     max-width: 1000px; z-index:499"))),
      flex = c(1, 5)
    )
  )
}


# Server ------------------------------------------------------------------

stories_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Sidebar
    sidebar_server("sidebar", "stories")

    # Map
    output$map <- renderMapdeck({
        mapdeck(style = map_style, token = token_stories, zoom = 10.5,
               location = map_location)
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
        rv_stories$zoom <- case_when(input$map_view_change$zoom > 13 ~ 600,
                                     TRUE ~ input$map_view_change$zoom*-550+8000)
      })
    
    
    # Update buffer to change the map when zoom is different
    data <- reactive({
        stories %>%
        mutate(buffer = st_buffer(geometry, rv_stories$zoom)) %>%
        st_set_geometry("buffer")
    })
    
    observeEvent({
      data()
      rv_stories$zoom}, {

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
      click <- jsonlite::fromJSON(input$map_polygon_click)$object$properties$id
      if (is.null(click)) {
        rv_stories$poly_selected <- NA
      } else if (!is.na(rv_stories$poly_selected) && 
                 click == rv_stories$poly_selected) {
        rv_stories$poly_selected <- NA
      } else rv_stories$poly_selected <- click
    })
    
    # Render the story in question, now only in english (_en)
    output$stories <- renderUI({
      
      if(!is.na(rv_stories$poly_selected)) {
        
      rmd_name <- stories[stories$ID == rv_stories$poly_selected,]$rmd
      bandeau_name <- stories[stories$ID == rv_stories$poly_selected,]$img
      
      HTML('<div class = "main_panel_text_popup">',
           # Adding bandeau img after the first div (title)
           str_replace(includeHTML(paste0("www/stories/", rmd_name, "_en.html")),
                       "</div>", paste0("</div><img src =", "stories/bandeau_img/",
                       bandeau_name,"><br><br>"))
           ,
           '</div>')
      }
      
    })
    
    # Anytime to "Go back to map" button is clicked, poly_selected goes
    # NA causing the div to disappear
    observeEvent(input$back, {
      rv_stories$poly_selected <- NA
    })
    
    
    observe({

      shinyjs::toggle("hr", condition = !is.na(rv_stories$poly_selected))
      shinyjs::toggle("back", condition = !is.na(rv_stories$poly_selected))
      shinyjs::toggle("stories", condition = !is.na(rv_stories$poly_selected))
      
    })
    
  })
}
