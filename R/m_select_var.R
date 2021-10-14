#### SELECT VARIABLE MODULE ####################################################

select_var_UI <- function(id, var_list) {
  pickerInput(NS(id, "var"), label = NULL, choices = var_list)
}

select_var_server <- function(id, var_list, disabled_choices = NULL) {
  stopifnot(is.reactive(var_list))
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      if (!is.null(disabled_choices)) {
        updatePickerInput(session, "var", 
                          choices = sus_translate(var_list()),
                          choicesOpt = list(
                            disabled = disabled_choices(),
                            style = ifelse(disabled_choices(),
                                           yes = "color: rgba(119, 119, 119, 0.5);",
                                           no = "")),
        )
      } else updatePickerInput(session, "var", 
                               choices = sus_translate(var_list()))
      
    })
    
    reactive(input$var)
    
  })
}
