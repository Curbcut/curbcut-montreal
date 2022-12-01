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

zoom_server <- function(id, r = r, zoom_string, zoom_levels) {
  
  stopifnot(is.reactive(zoom_string))
  stopifnot(is.reactive(zoom_levels))
  
  moduleServer(id, function(input, output, session) {
    
    # Disable the slider if in auto mode
    observeEvent(input$zoom_auto, {
      toggleState(id = "zoom_slider", condition = !input$zoom_auto)
    })
    
    # Update the slider if zoom_levels changes
    observeEvent({zoom_levels()
      r$lang()}, {
      updateSliderTextInput(session, "zoom_slider", 
                            selected = cc_t(r = r, get_zoom_name(zoom_string())),
                            choices = get_zoom_label_t({
                              # If the module isn't impacted by a change of r$geo()
                              if (is.list(zoom_levels())) {
                                zoom_levels()$levels
                              } else zoom_levels()}, 
                              r = r))
    })
    
    # Update the slider when zomo changes, only on auto_zoom
    observeEvent(zoom_string(), {
      if (input$zoom_auto)
        updateSliderTextInput(session, "zoom_slider", 
                              selected = cc_t(r = r, get_zoom_name(zoom_string())))
    }, priority = -1)
    
    # Update the slider if in auto mode
    observeEvent(zoom_string(), {
      if (input$zoom_auto) updateSliderTextInput(
        session, "zoom_slider", 
        selected = get_zoom_name(zoom_string()))
    })

    zoom_out <- reactive({
      out <- 
        if (input$zoom_auto) "auto_zoom" else get_zoom_code(input$zoom_slider)
      
      # If the module isn't impacted by a change of r$geo()
      if (!is.list(zoom_levels())) return(out)
      
      # If the module IS impacted by a change of r$geo()
        # Go back to auto_zoom if the constructed df() does not exist, which
        # happen when r$geo() is changed on a manual auto-zoom. The output will
        # be changed for a split second.
      if (is.null(get0(paste(zoom_levels()$region, out, sep = "_"))) && 
          !grepl("building", paste(zoom_levels()$region, out, sep = "_"))) 
        out <- "auto_zoom"
      return(paste(zoom_levels()$region, out, sep = "_"))
    })
    
    # Return value    
    return(zoom_out)
  })
}
