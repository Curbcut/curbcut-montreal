#### SLIDER MODULE ########################################################

# UI ----------------------------------------------------------------------

slider_UI <- function(id, slider_id = "slider", 
                      label = curbcut::cc_t("Select a year"), min = census_min, 
                      max = census_max, step = 5, sep = "", value = census_max,
                      width = "95%", ...) {
  
  sliderInput(NS(id, slider_id), label, min = min, max = max, step = step,
              sep = sep, value = value, width = width,  ...)
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

