### STORIES MODULE ##############################################################

# UI ----------------------------------------------------------------------

stories_UI <- function(id) {
  tabItem(tabName = "stories",
          shinyjs::useShinyjs(),
          shinyjs::hidden(htmlOutput(NS(id, "stories"),
                                     style = "position:absolute; margin: 40px;
                     max-width: 1000px; z-index:499")),
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   actionLink(NS(id, "back"), "Back to the map")
          ))
}


# Server ------------------------------------------------------------------

stories_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "stories")
    
    output$eso <- renderText(jsonlite::fromJSON(input$map_polygon_click))
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_stories, zoom = 11, 
              location = map_location) %>% 
        add_polygon(stories,
                    id = "ID", fill_opacity = 0, layer_id = "clickable",
                    stroke_opacity = 0) %>% 
        add_bitmap(paste0(stories_img_path, stories$img[1]),
                   bounds = as.vector(st_bbox(stories$buffer[1])),
                   layer_id = "image1") %>% 
        add_bitmap(paste0(stories_img_path, stories$img[2]),
                   bounds = as.vector(st_bbox(stories$buffer[2])),
                   layer_id = "image2") %>% 
        add_bitmap(paste0(stories_img_path, stories$img[3]),
                   bounds = as.vector(st_bbox(stories$buffer[3])),
                   layer_id = "image3") %>% 
        add_bitmap(paste0(stories_img_path, stories$img[4]),
                   bounds = as.vector(st_bbox(stories$buffer[4])),
                   layer_id = "image4")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_stories$zoom <- input$map_view_change$zoom*-550+8000
    })
    
    
    # Update buffer to change the map when zoom is different
    data <- reactive({
        stories %>%
        mutate(buffer = st_buffer(geometry, rv_stories$zoom)) %>%
        st_set_geometry("buffer")
    })
    
    # Update map when data changes due to the zoom
    observeEvent({
      data()
      rv_canale$zoom}, {
      mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(data(),
                      id = "ID", fill_opacity = 0, layer_id = "clickable",
                      stroke_opacity = 0) %>% 
          add_bitmap(paste0(stories_img_path, data()$img[1]),
                     bounds = as.vector(st_bbox(data()$buffer[1])),
                     layer_id = "image1") %>% 
          add_bitmap(paste0(stories_img_path, data()$img[2]),
                     bounds = as.vector(st_bbox(data()$buffer[2])),
                     layer_id = "image2") %>% 
          add_bitmap(paste0(stories_img_path, data()$img[3]),
                     bounds = as.vector(st_bbox(data()$buffer[3])),
                     layer_id = "image3") %>% 
          add_bitmap(paste0(stories_img_path, data()$img[4]),
                     bounds = as.vector(st_bbox(data()$buffer[4])),
                     layer_id = "image4")
        
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
      
      HTML('<div id = "main_panel_text_popup">',
           # Adding bandeau img after the first div (title)
           str_replace(includeHTML(paste0("www/stories/", rmd_name, "_en.html")),
                       "</div>", paste0("</div><img src =", "stories/bandeau_img/",
                       bandeau_name,"><br>"))
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

      shinyjs::toggle("title-title", condition = is.na(rv_stories$poly_selected))
      shinyjs::toggle("title-title_extra", condition = is.na(rv_stories$poly_selected))
      shinyjs::toggle("title-title_main", condition = is.na(rv_stories$poly_selected))
      shinyjs::toggle("title-more_info", condition = is.na(rv_stories$poly_selected))
      shinyjs::toggle("title-more_info", condition = is.na(rv_stories$poly_selected))
      shinyjs::toggle("back", condition = !is.na(rv_stories$poly_selected))
      shinyjs::toggle("stories", condition = !is.na(rv_stories$poly_selected))
      
    })
    
  })
}
