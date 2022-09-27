#### SELECT VARIABLE MODULE ####################################################

select_var_UI <- function(id, select_var_id = "var", 
                          var_list, label = NULL, width = "100%", 
                          inline = FALSE, more_style = NULL, selected = NULL) {
  style <- ""
  if (inline) style <- paste("display: inline-block;", style)
  if (!is.null(more_style)) style <- paste(style, more_style)
  
  div(style = style,
      pickerInput(NS(id, select_var_id), label = label, choices = var_list, 
                  selected = selected, width = width))
}

select_var_server <- function(id, r = r, select_var_id = "var",
                              var_list, disabled = reactive(NULL), 
                              time = reactive(NULL), df = r[[id]]$df) {
  
  stopifnot(is.reactive(var_list))
  stopifnot(is.reactive(time))

  moduleServer(id, function(input, output, session) {
    
    # Update dropdown menu if there are disabled choices
    observe({
      if (!is.null(disabled())) {
        updatePickerInput(
          session, select_var_id, 
          choices = sus_translate(r = r, var_list()),
          choicesOpt = list(disabled = disabled(),
                            style = ifelse(disabled(), 
                                           "color: rgba(119, 119, 119, 0.5);", 
                                           "")))
      } else {
        updatePickerInput(
        session, select_var_id, 
        choices = sus_translate(r = r, var_list()))
      }
      })

    var <- reactive({
      if (input[[select_var_id]] == " ") return(" ")
      v1 <- paste(input[[select_var_id]], time(), sep = "_")
      v1 <- sub("_$", "", v1)
      if (!is.null(time()) && !is.null(df) && 
          is_scale_in_df(c(all_choropleth, "grid"), 
                         df())) {
        v1 <- sapply(v1, return_closest_year, df(), USE.NAMES = FALSE)
      }
      v1 <- ifelse(str_detect(v1, "^_\\d{4}$"), " ", v1)
      if (all(v1 == " ")) v1 <- " "
      v1
      })
    
    var
    
  })
}
