#### SELECT VARIABLE MODULE ####################################################

select_var_UI <- function(id, var_list, label = NULL, width = NULL) {
  pickerInput(NS(id, "var"), label = label, choices = var_list, width = width)
}

select_var_server <- function(id, var_list, disabled_choices = NULL, 
                              time = reactive(NULL), df = NULL) {
  stopifnot(is.reactive(var_list))
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      if (!is.null(disabled_choices)) {
        updatePickerInput(
          session, "var", 
          choices = sus_translate(var_list()),
          choicesOpt = list(disabled = disabled_choices(),
                            style = ifelse(
                              disabled_choices(),
                              yes = "color: rgba(119, 119, 119, 0.5);",
                              no = "")))
      } else updatePickerInput(session, "var", 
                               choices = sus_translate(var_list()))
      
    })
    
    var <- reactive({
      v1 <- paste(input$var, time(), sep = "_")
      v1 <- sub("_$", "", v1)
      if (!is.null(df)) v1 <- sapply(v1, return_closest_year, df)
      unique(v1)
    })
    
    var
    
  })
}
