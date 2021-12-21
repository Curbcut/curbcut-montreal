add_variables <- function(data, var_code, var_title, var_short, explanation, 
                          category, private, dates, scales, breaks_q3, 
                          breaks_q5, source) {
  
  # var_short check
  if (str_length(var_short) > 12) stop("`var_short` is more than 12 characters")
  
  add_row(data,
          var_code = var_code,
          var_title = var_title,
          var_short = var_short,
          explanation = explanation,
          category = category,
          private = private,
          dates = list(dates),
          scales = list(scales),
          breaks_q3 = list(breaks_q3),
          breaks_q5 = list(breaks_q5),
          source = source)
  
}