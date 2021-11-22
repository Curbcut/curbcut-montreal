#### ZOOM MODULE ###############################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param ... Name-value pairs whose names are the name of the zoom level and
#' whose values are the minimum zoom value at which the zoom level should be
#' activated.
#' @return A reactive expression containing a length-one character string.

zoom_UI <- function(id, zoom_levels) {
  div(style = "font-size: 11px; vertical-align: bottom;",
      div(checkboxInput(
        inputId = NS(id, "auto"), 
        label = "Auto-zoom", 
        value = TRUE, 
        width = "50px"),
        style = "display: inline-block; padding: 5px; vertical-align: bottom;"),
      div(sliderTextInput(
        inputId = NS(id, "slider"), 
        label = NULL, 
        choices = get_zoom_label(zoom_levels), 
        hide_min_max = TRUE, 
        force_edges = TRUE, 
        width = "150px"), 
        style = "display: inline-block; padding: 5px; vertical-align: bottom;")
  )
}

zoom_server <- function(id, zoom, zoom_levels) {
  
  stopifnot(is.reactive(zoom))
  stopifnot(!is.reactive(zoom_levels))
  
  moduleServer(id, function(input, output, session) {

    # Disable the slider if in auto mode
    observeEvent(input$auto, {
      toggleState(id = "slider", condition = !input$auto)
    })
    
    # Update the slider if in auto mode
    observeEvent(zoom(), {
      if (input$auto) updateSliderTextInput(session, "slider", 
                                            selected = get_zoom_name(zoom()))
    })
    
    # Get slider value
    zoom_out <- reactive({
      if (input$auto) zoom() else get_zoom_code(input$slider)
      })
    
    zoom_out
  })
}
