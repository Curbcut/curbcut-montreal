#### CHECKBOX MODULE ########################################################

# UI ----------------------------------------------------------------------

checkbox_UI <- function(id, checkbox_id = NULL, ...) {
  
  checkbox_id <- if (is.null(checkbox_id)) "cbox" else checkbox_id
  
  checkboxInput(NS(id, checkbox_id), ...)
  
}


# Server ------------------------------------------------------------------

checkbox_server <- function(id, checkbox_id = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    checkbox_id <- if (is.null(checkbox_id)) "cbox" else checkbox_id
    
    reactive(input[[checkbox_id]])
    
  })
  
}