#### SELECT VARIABLE MODULE ####################################################

select_var_UI <- function(id, var_list, label = NULL, width = "100%", 
                          inline = FALSE, more_style = NULL, selected = NULL) {
  style <- ""
  if (inline) style <- paste("display: inline-block;", style)
  if (!is.null(more_style)) style <- paste(style, more_style)
  
  div(style = style,
      pickerInput(NS(id, "var"), label = label, choices = var_list, 
                  selected = selected, width = width))
}

select_var_server <- function(id, var_list, disabled = reactive(NULL), 
                              time = reactive(NULL), df = reactive(NULL)) {
  
  stopifnot(is.reactive(var_list))
  stopifnot(is.reactive(time))
  stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    
    # Update dropdown menu if there are disabled choices
    observe({
      if (!is.null(disabled())) {
        updatePickerInput(
          session, "var", 
          choices = sus_translate(var_list()),
          choicesOpt = list(disabled = disabled(),
                            style = ifelse(disabled(), 
                                           "color: rgba(119, 119, 119, 0.5);", 
                                           "")))
      }})
    
    var <- reactive({
      v1 <- paste(input$var, time(), sep = "_")
      v1 <- sub("_$", "", v1)
      if (!is.null(df()) && df() %in% c("borough", "CT", "DA", "grid")) v1 <- 
        sapply(v1, return_closest_year, df(), USE.NAMES = FALSE)
      v1 <- if_else(str_detect(v1, "^ _\\d{4}$"), " ", v1)
      if (all(v1 == " ")) v1 <- " "
      v1})
    
    var
    
  })
}
