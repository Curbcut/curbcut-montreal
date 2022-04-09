#### GET PLACE EXPLORER BLOCK TEXT #############################################

get_pe_block_text <- function(df, theme, select_id, island_or_region,
                              data_order) {
  
  if (length(data_order$var_code) == 0) return(data.frame())
  data_ord <- data_order
  vars <- variables
  
  # Access for CT
  if (df == "CT" && theme == "Transport") {
    data_ord <- unique(data_ord)
    vars <- get_CT_access_vars(variables)
  }
  
  vars_theme <- vars[vars$var_code %in% data_ord$var_code, ]
  vars_theme <- vars_theme[order(match(vars_theme$var_code, data_ord$var_code)),
                           c("var_title", "explanation")]
  
  data_ord <- cbind(data_ord, vars_theme)
  
  data_var <- pe_var_hierarchy[[df]][names(pe_var_hierarchy[[df]]) %in%
                                       data_ord$var_code]
  data_var <- data_var[order(match(names(data_var), data_ord$var_code))]
  data_var <- lapply(data_var, \(x) x[x$ID == select_id, ])
  
  col <- paste0(island_or_region, "_percentile")
  data_var <- data.frame(
      percentile = sapply(data_var, \(x) x[[col]], USE.NAMES = FALSE),
      var_code = names(data_var),
      value = sapply(data_var, \(x) x$var, USE.NAMES = FALSE),
      row.names = NULL)
  
  out <- cbind(data_ord, data_var)
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
