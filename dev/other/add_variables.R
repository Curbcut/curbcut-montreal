add_variables <- function(data, var_code, var_title, var_short, explanation, 
                          category, theme, private, dates, scales, breaks_q3, 
                          breaks_q5, source, interpolated, grouping = NA_character_,
                          group_diff = NA_character_) {
  
  if (var_code %in% data$var_code) 
    stop(paste0("`", var_code, "` is a duplicate."))
  
  add_row(data,
          var_code = var_code,
          var_title = as.character(var_title),
          var_short = 
            if (is.na(var_short)) as.character(var_title) else as.character(var_short),
          explanation = as.character(explanation),
          category = category,
          theme = theme,
          private = private,
          dates = list(dates),
          scales = list(scales),
          breaks_q3 = list(breaks_q3),
          breaks_q5 = list(breaks_q5),
          source = source,
          interpolated = list(interpolated),
          grouping = grouping,
          group_diff = list(group_diff))
  
}