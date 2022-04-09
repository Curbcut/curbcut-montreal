#### GET PLACE EXPLORER BLOCK ##################################################

get_pe_block <- function(df, theme, select_id, island_or_region) {
  
  ## Get data ------------------------------------------------------------------
  
  data_order <- pe_variable_order[[df]]
  data_order <- data_order[[island_or_region]]
  data_order <- data_order[data_order$theme == theme,]
  data_order <- data_order[data_order$ID == select_id, "var_code"]
  data_order <- unique(data_order)
  
  # Exit early if there is no data
  if (length(data_order$var_code) == 0) return(NULL)
  
  vars <- if (df == "CT" && theme == "Transport") {
    get_CT_access_vars(variables)
  } else variables
  
  vars_theme <- vars[vars$var_code %in% data_order$var_code, ]
  var_match <- match(vars_theme$var_code, data_order$var_code)
  vars_theme <- vars_theme[order(var_match), c("var_title", "explanation")]
  
  data_order <- cbind(data_order, vars_theme)
  
  data_var <- pe_var_hierarchy[[df]]
  data_var <- data_var[names(data_var) %in% data_order$var_code]
  data_var <- data_var[order(match(names(data_var), data_order$var_code))]
  
  
  ## Get block text ------------------------------------------------------------
  
  data_sel <- lapply(data_var, \(x) x[x$ID == select_id, ])
  
  col <- paste0(island_or_region, "_percentile")
  block_text <- data.frame(
    percentile = sapply(data_sel, \(x) x[[col]], USE.NAMES = FALSE),
    var_code = names(data_sel),
    value = sapply(data_sel, \(x) x$var, USE.NAMES = FALSE),
    row.names = NULL)
  
  block_text <- cbind(data_order, block_text)
  block_text <- block_text[!is.na(block_text$value), ]
  
  block_text$percentile <- sapply(block_text$percentile, \(percentiles) {
    if (percentiles > 0.50) {
      per <- scales::percent(abs(percentiles - 1))
      if (per == "0%") per <- "1%"
      sus_translate("Top {per}")
    } else {
      per <- scales::percent(abs(percentiles))
      if (per == "0%") per <- "1%"
      sus_translate("Bottom {per}")
    }
  })
  
  # Pretty grid output
  block_text$value <- mapply(convert_unit, block_text$value, 
                             block_text$var_code, USE.NAMES = FALSE)
  
  block_text <- block_text[c("var_title", "explanation", "percentile", "value")]
  
  
  ## Get block plots -----------------------------------------------------------
  
  # Plot
  plots <- lapply(data_var, \(data) {
    
    hex_to_plot <- "#A9A9A9"
    
    data_sel <- data$var[data$ID == select_id]
    
    if (!is.na(data_sel)) {
      
      dat <- data[!is.na(data$var),]
      outliers <- find_outliers(dat$var)
      if (length(outliers) > 0 && !data_sel %in% dat$var[outliers]) {
        dat <- dat[-outliers,]
      }
      
      ggplot(dat) +
        geom_density(aes(x = var), size = 1, color = hex_to_plot) +
        geom_vline(aes(xintercept = data_sel), color = "#000000", size = 1,
                   alpha = 1) +
        theme_void()
    } else ggplot() + theme_void()
    
  })
  
  
  ## Get sentence --------------------------------------------------------------
  
  col <- paste0(island_or_region, "_percentile")
  out <- data.frame(
    percentile = sapply(data_sel, \(x) x[[col]], USE.NAMES = FALSE),
    var_code = names(data_sel),
    value = sapply(data_sel, \(x) x$var, USE.NAMES = FALSE),
    row.names = NULL)
  
  out <- cbind(data_order, out)
  out <- out[!is.na(out$value), ]
  ior <- sus_translate("the ", island_or_region)
  
  # Age
  sentence <- if (theme == "Age") {
    
    z <- out[out$var_code %in% c("age_65_plus_pct", "age_0_14_pct"), ]
    z <- z[z$percentile == max(z$percentile), ]
    if (z$percentile > 0.5) {
      older_younger <- 
        if (z$var_code == "age_65_plus_pct") sus_translate("older") else 
          sus_translate("younger")
      sus_translate("The area's population is {older_younger} than average ",
                    "for {ior}.")
    } else NULL
    
    # Climate risk  
  } else if (theme == "Climate risk") {
    
    z <- mean(out$percentile)
    more_less <- if (z >= 0.8) {
      sus_translate("much higher")
    } else if (z >= 0.6) {
      sus_translate("higher")
    } else if (z >= 0.5) {
      sus_translate("slightly higher")
    } else if (z >= 0.4) {
      sus_translate("slightly lower")
    } else if (z >= 0.2) {
      sus_translate("lower")
    } else sus_translate("much lower")
    sus_translate("The area has a {more_less} level of climate risk than ",
                  "average for {ior}.")
    
    # Identity
  } else if (theme == "Identity") {
    
    z <- out[out$var_code == "iden_imm_pct", ]
    more_less <- if (z$percentile > 0.5) sus_translate("more") else 
      sus_translate("fewer")
    sus_translate("The area has {more_less} foreign-born residents than ",
                  "average for {ior}.")
    
    # Income
  } else if (theme == "Income") {
    
    z <- out[out$var_code %in% c("inc_limat_pct", "inc_high_pct"), ]
    z <- z[z$percentile == max(z$percentile), ]
    
    if (z$percentile > 0.5) {
      higher_lower <- 
        if (z$var_code == "inc_high_pct") sus_translate("higher") else 
          sus_translate("lower")
      sus_translate("Incomes in the area are {higher_lower} than ",
                    "average for {ior}.")
    } else NULL
    
    # Language
  } else if (theme == "Language") {
    
    z <- out[out$var_code %in% c("lang_eng_only_pct", "lang_french_only_pct",
                                 "lang_french_eng_pct"), ]
    z <- z[z$percentile == max(z$percentile), ]
    
    if (z$percentile > 0.5) {
      lang <- {
        if (z$var_code == "lang_eng_only_pct") {
          sus_translate("speak more English")
        } else if (z$var_code == "lang_french_only_pct") {
          sus_translate("speak more French")
        } else sus_translate("are more bilingual (French and English)")
      }
      sus_translate("The area's residents {lang} than average for ",
                    "{ior}.")
    } else NULL
    
    # Education
  } else if (theme == "Education") {
    
    z <- out[out$percentile == max(out$percentile), ]
    
    if (z$percentile > 0.5) {
      more_less <- {
        if (z$var_code == "edu_bachelor_above_pct") sus_translate("higher") else
          sus_translate("lower")
      }
      sus_translate("Residents of the area have a {more_less} proportion of ",
                    "university degrees than average for ",
                    "{ior}.")
    } else NULL
    
    # Housing
  } else if (theme == "Housing") {
    
    z <- out[out$var_code %in% c("housing_tenant_pct", 
                                 "housing_value_avg_dollar"), ]
    z <- z[z$percentile == max(z$percentile), ]
    
    if (z$percentile > 0.5) {
      if (z$var_code == "housing_value_avg_dollar") {
        sus_translate("Property values in the area are higher than average ",
                      "for {ior}.")
      } else {
        sus_translate("The area's percentage of tenants is higher than ",
                      "average for {ior}.")
      }
    } else NULL
    
    # Transport
  } else if (theme == "Transport") {
    
    z <- out$percentile[out$var_code == "trans_car_pct"]

    more_less <- if (z >= 0.8) {
      sus_translate("much more")
    } else if (z >= 0.6) {
      sus_translate("more")
    } else if (z >= 0.5) {
      sus_translate("slightly more")
    } else if (z >= 0.4) {
      sus_translate("slightly less")
    } else if (z >= 0.2) {
      sus_translate("less")
    } else sus_translate("much less")
    
    sus_translate("Residents in the area drive to work {more_less} than ",
                  "average for {ior}.")
  } else NULL
  
  
  ## Combine and return output -------------------------------------------------
  
  return(list(block_text, plots, sentence))
  
}