#### SLIDER MODULE ########################################################

# UI ----------------------------------------------------------------------

slider_UI <- function(id, slider_id = NULL, 
                      label = sus_translate("Select a year"), min = census_min, 
                      max = census_max, step = 5, sep = "", value = census_max,
                      width = "95%", ...) {
  
  slider_id <- if (is.null(slider_id)) "slider" else slider_id
  
  sliderInput(NS(id, slider_id), label, min = min, max = max, step = step,
              sep = sep, value = value, width = width,  ...)
}


# Server ------------------------------------------------------------------

slider_server <- function(id, slider_id = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    slider_id <- if (is.null(slider_id)) "slider" else slider_id
    
    reactive(input[[slider_id]])
    
  })
  
}