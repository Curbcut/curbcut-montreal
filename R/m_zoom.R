#### ZOOM MODULE ###############################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param zoom A reactive value which resolves to a length-one character vector
#' @param zoom_levels A named numeric vector whose names are the name of the 
#' zoom level and whose values are the minimum zoom value at which the zoom 
#' level should be activated.
#' @return A reactive expression containing a length-one character string.

zoom_UI <- function(id, zoom_levels) {
  
  tagList(
      div(class = "sus-sidebar-control", checkboxInput(
        inputId = NS(id, "zoom_auto"), 
        label = "Auto-zoom", 
        value = TRUE)),
      div(class = "sus-sidebar-control", sliderTextInput(
        inputId = NS(id, "zoom_slider"), 
        label = NULL, 
        choices = get_zoom_label(zoom_levels), 
        hide_min_max = TRUE, 
        force_edges = TRUE))
  )
  
}

zoom_server <- function(id, zoom_string, zoom_levels) {
  
  stopifnot(is.reactive(zoom_string))
  stopifnot(is.reactive(zoom_levels))
  
  moduleServer(id, function(input, output, session) {
    
    # Disable the slider if in auto mode
    observeEvent(input$zoom_auto, {
      toggleState(id = "zoom_slider", condition = !input$zoom_auto)
    })
    
    # Update the slider if zoom_levels changes
    observeEvent({zoom_levels()
      sus_rv$lang()}, {
      updateSliderTextInput(session, "zoom_slider", 
                            selected = sus_translate(get_zoom_name(zoom_string())),
                            choices = get_zoom_label_t(zoom_levels()))
    })
    
    # Update the slider when zomo changes, only on auto_zoom
    observeEvent(zoom_string(), {
      if (input$zoom_auto)
        updateSliderTextInput(session, "zoom_slider", 
                              selected = sus_translate(get_zoom_name(zoom_string())))
    }, priority = -1)
    
    # Update the slider if in auto mode
    observeEvent(zoom_string(), {
      if (input$zoom_auto) updateSliderTextInput(
        session, "zoom_slider", 
        selected = get_zoom_name(zoom_string()))
    })
    
    # Get slider value
    zoom_out <- reactive({
      if (input$zoom_auto) "auto_zoom" else get_zoom_code(input$zoom_slider)
      })
    
    # Return value    
    return(zoom_out)
  })
}
