#### ZOOM MODULE ###############################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param ... Name-value pairs whose names are the name of the zoom level and
#' whose values are the minimum zoom value at which the zoom level should be
#' activated.
#' @return A reactive expression containing a length-one character string.

zoom_UI <- function(id, zoom_levels) {
  div(style = "font-size: 11px; vertical-align: bottom;",
      # h5("Zoom", style = "font-size: 11px;"),
      div(checkboxInput(NS(id, "zoom_auto"), "Auto-zoom", value = TRUE,
                        width = "50px"),
          style = "display: inline-block; padding: 5px; vertical-align: bottom;"),
      div(sliderTextInput(NS(id, "zoom"), label = NULL, hide_min_max = TRUE, 
                          choices = get_zoom_label(zoom_levels), 
                          force_edges = TRUE, width = "150px"), 
          style = "display: inline-block; padding: 5px; vertical-align: bottom;")
  )
}

zoom_server <- function(id, zoom, zoom_levels, ...) {
  stopifnot(is.reactive(zoom),
            !is.reactive(zoom_levels))
  
  moduleServer(id, function(input, output, session) {
    zm <- reactive(get_zoom(zoom(), zoom_levels))

    observeEvent(zm(), {
      print("ZOOM NAME")
      print(zm())
      updateSliderTextInput(session, "zoom", selected = get_zoom_name(zm()))
    })
    
      # print("ZOOM TEST")
      # print(zoom_test)
      # zoom_test
    reactive()
  })
}
