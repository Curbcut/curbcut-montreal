#### SELECT VARIABLE MODULE ####################################################

# Create divs for var_list (text on hover) --------------------------------

hover_divs <- function(var_list, r = NULL) {
  
  text_hover <- 
    unname(sapply(unlist(var_list), \(x) {
      exp <- variables$explanation[variables$var_code == x]
      if (length(exp) == 0) return(" ")
      return(exp)}))
  
  if (!is.null(r)) 
    text_hover <- sapply(text_hover, cc_t, r = r, USE.NAMES = FALSE)
  
  var_list_label <- if (vec_dep(var_list) == 3) {
    c(if ("----" %in% names(var_list)) "----",
      unname(unlist(sapply(var_list, names))))
  } else {
    c(if ("----" %in% names(var_list)) "----",
      unname(unlist(names(var_list))))
  }
  value <- unname(unlist(var_list))
  
  return(list(
    content = sprintf(
      '<div title="%s" value="%s" style="width: 100%%;">%s</div>',
      text_hover, value, var_list_label
    )
  ))
}


# Ui ----------------------------------------------------------------------

select_var_UI <- function(id, select_var_id = "var", 
                          var_list, label = NULL, width = "100%", 
                          inline = FALSE, more_style = NULL, selected = NULL) {
  style <- ""
  if (inline) style <- paste("display: inline-block;", style)
  if (!is.null(more_style)) style <- paste(style, more_style)

  div(style = style,
      pickerInput(NS(id, select_var_id), label = label, choices = var_list, 
                  selected = selected, width = width,
                  choicesOpt = hover_divs(var_list)
                  ))
}


# Server ------------------------------------------------------------------

select_var_server <- function(id, r, select_var_id = "var",
                              var_list, disabled = reactive(NULL), 
                              time = reactive(NULL), df = r[[id]]$df) {
  
  stopifnot(is.reactive(var_list))
  stopifnot(is.reactive(time))

  moduleServer(id, function(input, output, session) {
    
    t_var_list <- reactive(cc_t(r = r, var_list()))
    
    # Update dropdown menu if there are disabled choices
    observe({
      
      if (!is.null(disabled())) {
        updatePickerInput(
          session, select_var_id,
          choices = t_var_list(),
          choicesOpt = c(hover_divs(t_var_list(), r = r),
                         list(disabled = disabled(),
                            style = ifelse(disabled(),
                                           "color: rgba(119, 119, 119, 0.5);",
                                           ""))))
      } else {

        # Disable in the case variable isn't available at all years
        v <- variables[variables$var_code %in% unlist(var_list()),]
        add_disabled <- if (!all(is.na(unlist(v$dates))) && length(time()) != 1) {
          unlist(var_list()) %in% 
            v$var_code[!sapply(v$dates, length) == max(sapply(v$dates, length))]
        } else NULL
        
        choicesOpt <- if (!is.null(add_disabled)) {
          choicesOpt <-  c(hover_divs(t_var_list(), r = r),
                           list(disabled = add_disabled,
                                style = ifelse(add_disabled,
                                               "color: rgba(119, 119, 119, 0.5);",
                                               "")))
        } else hover_divs(t_var_list(), r = r)

        updatePickerInput(
          session, select_var_id,
          choices = t_var_list(),
          choicesOpt = choicesOpt)
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
