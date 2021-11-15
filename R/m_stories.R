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
    
    output$eso <- renderText(paste0(rv_stories$poly_selected))
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(style = map_style, token = token_stories, zoom = 11, 
              location = map_location) %>% 
        add_polygon(stories,
                    id = "ID", fill_opacity = 0, layer_id = "clickable",
                    stroke_opacity = 0
        ) %>% 
        add_bitmap(paste0(stories_img_path, stories$img),
                   bounds = as.vector(st_bbox(stories$buffer)),
                   layer_id = "images")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_stories$zoom <- case_when(input$map_view_change$zoom >= 13 ~ 500,
                                 input$map_view_change$zoom >= 12 ~ 1000,
                                 input$map_view_change$zoom >= 11 ~ 1500,
                                 TRUE ~ 2000)
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
        add_polygon(data = data(),
                    id = "ID", fill_opacity = 0,
                    stroke_opacity = 0,
                    layer_id = "clickable"
         ) %>% 
          add_bitmap(paste0(stories_img_path, data()$img),
                     bounds = as.vector(st_bbox(data()$buffer[1])),
                     layer_id = "images")
        
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
        
      rmd_name <- stories[stories$ID == rv_stories$poly_selected]$rmd
      
      HTML('<div id = "main_panel_text_popup">',
           includeHTML(paste0("www/stories/", rmd_name, "_en.html")),
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
