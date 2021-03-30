#### SELECT VARIABLE MODULE ####################################################

select_var_UI <- function(id, var_list) {
  selectInput(NS(id, "var"), label = NULL, choices = var_list)
}

select_var_server <- function(id, var_list) {
  stopifnot(!is.reactive(var_list))
  
  moduleServer(id, function(input, output, session) {
    observe({updateSelectInput(session, "var", 
                               choices = sus_translate(var_list))})
    reactive(input$var)
  })
}
