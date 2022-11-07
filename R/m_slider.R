#### SLIDER MODULE ########################################################

# UI ----------------------------------------------------------------------

slider_UI <- function(id, slider_id = "slider", 
                      label = cc_t(r = r, "Select a year"), min = census_min, 
                      max = census_max, step = 5, sep = "", value = census_max,
                      width = "95%", ...) {
  
  sliderInput(NS(id, slider_id), label, min = min, max = max, step = step,
              sep = sep, value = value, width = width,  ...)
}

slider_text_UI <- function(id, slider_id = "slider", 
                           label = cc_t(r = r, "Select a year"), 
                           choices, selected = NULL, width = "95%", ...) {
  
  shinyWidgets::sliderTextInput(NS(id, slider_id), label, choices = choices,
                                selected = selected, width = width,  ...)
}


# Server ------------------------------------------------------------------

slider_server <- function(id, slider_id = NULL, value = reactive(NULL),
                          min = reactive(NULL), max = reactive(NULL)) {
  
  stopifnot(is.reactive(value))
  stopifnot(is.reactive(min))
  stopifnot(is.reactive(max))
  
  moduleServer(id, function(input, output, session) {
    
    slider_id <- if (is.null(slider_id)) "slider" else slider_id
    
    observe({
      if (!is.null(value()) || !is.null(min()) || !is.null(max())) {
        updateSliderInput(
          session,
          inputId = slider_id,
          value = value(),
          min = min(),
          max = max())
      }
    })
    
    reactive(input[[slider_id]])
    
  })
  
}

slider_text_server <- function(id, r = r, slider_id = NULL, choices = reactive(NULL),
                               selected = reactive(NULL)) {
  
  stopifnot(is.reactive(choices))
  stopifnot(is.reactive(selected))
  
  moduleServer(id, function(input, output, session) {
    
    slider_id <- if (is.null(slider_id)) "slider" else slider_id
    
    choices <- sapply(choices(), cc_t, r = r, USE.NAMES = FALSE)
    
    observe({
      if (!is.null(choices()) || !is.null(selected())) {
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = slider_id,
          selected = selected(),
          choices = choices)
      }
    })
    
    reactive(input[[slider_id]])
    
  })
  
}
