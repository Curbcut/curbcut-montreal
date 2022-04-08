#### GET PLACE EXPLORER BLOCK TEXT #############################################

get_pe_block_text <- function(df, theme, select_id, island_or_region,
                              data_order) {
  
  if (length(data_order$var_code) == 0) return(data.frame())
  
  if (df == "CT" && theme == "Transport")
    data_order <- unique(data_order)
  
  # Access for CT
  variables_var_codes <-
    if (df == "CT" && theme == "Transport") {
      rbind(
        variables[!grepl("access", variables$var_code), ], {
          
          access_vars <- variables[grepl("access", variables$var_code), ]
          
          new_var_code <- c(
            access_vars$var_code[
              str_starts(access_vars$var_code, "access_jobs")] |> 
              str_extract("access_jobs_[^_]*"),
            access_vars$var_code[
              !str_starts(access_vars$var_code, "access_jobs")] |> 
              str_extract("access_[^_]*"))
          
          access_vars$var_code <- new_var_code
          unique(access_vars, incomparables = FALSE, MARGIN = 2)
          access_vars <- access_vars[!duplicated(access_vars$var_code), ]
          
          exp_suffix <- c("at weekday peak service",
                          "at weekday off-peak service",
                          "at weekday night service",
                          "at weekend peak service",
                          "at weekend off-peak service",
                          "at weekend night service")
          
          access_vars$explanation <-
            str_replace(access_vars$explanation, 
                        paste0(exp_suffix, collapse = "|"),
                        "on average")
          
          access_vars
        }
      )
    } else variables
  
  variables_theme <- 
    variables_var_codes[variables_var_codes$var_code %in% data_order$var_code, ]
  variables_theme <-
    variables_theme[order(match(variables_theme$var_code, data_order$var_code)),
                    c("var_title", "explanation")]
  
  data_order <- cbind(data_order, variables_theme)
  
  raw_data_var <- pe_var_hierarchy[[df]][names(pe_var_hierarchy[[df]]) %in%
                                           data_order$var_code]
  raw_data_var <- raw_data_var[order(match(names(raw_data_var), 
                                           data_order$var_code))]
  data_var <- lapply(raw_data_var, \(x) x[x$ID == select_id, ])
  
  col <- paste0(island_or_region, "_percentile")
  data_var <- data.frame(
      col = sapply(data_var, \(x) x[[col]], USE.NAMES = FALSE),
      var_code = names(data_var),
      value = sapply(data_var, \(x) x$var, USE.NAMES = FALSE),
      row.names = NULL) |> 
    setNames(c("percentile", "var_code", "value"))
  
  out <- cbind(data_order, data_var)
  out <- out[!is.na(out$value), ]
  
  percentile <- sapply(out$percentile, \(out_percentiles) {
    if (out_percentiles > 0.50) {
      per <- scales::percent(abs(out_percentiles - 1))
      if (per == "0%") per <- "1%"
      sus_translate("Top {per}")
    } else {
      per <- scales::percent(abs(out_percentiles))
      if (per == "0%") per <- "1%"
      sus_translate("Bottom {per}")
    }
  })
  
  # Pretty grid output
  out$percentile <- percentile
  out$value <- mapply(convert_unit, out$value, out$var_code, USE.NAMES = FALSE)
  
  return(out[, c("var_title", "explanation", "percentile", "value")])
  
}
