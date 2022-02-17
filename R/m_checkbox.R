#### CHECKBOX MODULE ########################################################

# UI ----------------------------------------------------------------------

checkbox_UI <- function(id, ...) {
  
  checkboxInput(NS(id, "checkbox"), ...)
  
}


# Server ------------------------------------------------------------------

checkbox_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    reactive(input$checkbox)
    
  })
  
}