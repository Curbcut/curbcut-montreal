add_variables <- function(data, var_code, var_title, var_short, explanation, 
                          category, theme, private, dates, scales, breaks_q3, 
                          breaks_q5, source) {
  
  add_row(data,
          var_code = var_code,
          var_title = var_title,
          var_short = if (is.na(var_short)) var_title else var_short,
          explanation = explanation,
          category = category,
          theme = theme,
          private = private,
          dates = list(dates),
          scales = list(scales),
          breaks_q3 = list(breaks_q3),
          breaks_q5 = list(breaks_q5),
          source = source)
  
}