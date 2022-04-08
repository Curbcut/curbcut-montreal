#### GET PLACE EXPLORER BLOCK SENTENCE #########################################

get_pe_block_sentence <- function(df, theme, select_id, island_or_region,
                                  data_order) {
  
  if (df == "CT" && theme == "Transport") data_order <- unique(data_order)
  
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
  
  island_or_region <- sus_translate("the ", island_or_region)
  
  if (theme == "Age") {
    z <- out[out$var_code %in% c("age_65_plus_pct", "age_0_14_pct"), ]
    z <- z[z$percentile == max(z$percentile), ]
    if (z$percentile > 0.5) {
      older_younger <- 
        if (z$var_code == "age_65_plus_pct") sus_translate("older") else 
          sus_translate("younger")
      sus_translate("The area's population is {older_younger} than average ",
                    "for {island_or_region}.")
    } else NULL
    
  } else if (theme == "Identity") {
    z <- out[out$var_code == "iden_imm_pct", ]
    more_less <- if (z$percentile > 0.5) sus_translate("more") else 
      sus_translate("less")
    sus_translate("The area has {more_less} foreign-born residents than ",
                  "average for {island_or_region}.")
    
  } else if (theme == "Income") {
    z <- out[out$var_code %in% c("inc_limat_pct", "inc_high_pct"), ]
    z <- z[z$percentile == max(z$percentile), ]
    
    if (z$percentile > 0.5) {
      higher_lower <- 
        if (z$var_code == "inc_high_pct") sus_translate("higher") else 
          sus_translate("lower")
      sus_translate("Incomes in the area are {higher_lower} than ",
                    "average for {island_or_region}.")
    } else NULL
    
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
                    "{island_or_region}.")
    } else NULL
    
  } else if (theme == "Education") {
    z <- out[out$percentile == max(out$percentile), ]
    
    if (z$percentile > 0.5) {
      more_less <- {
        if (z$var_code == "edu_bachelor_above_pct") sus_translate("higher") else
          sus_translate("lower")
      }
      sus_translate("Residents of the area have a {more_less} proportion of ",
                    "university degrees than average for ",
                    "{island_or_region}.")
    } else NULL
    
  } else if (theme == "Housing") {
    z <- out[out$var_code %in% c("housing_tenant_pct", 
                                 "housing_value_avg_dollar"), ]
    z <- z[z$percentile == max(z$percentile), ]
    
    if (z$percentile > 0.5) {
      if (z$var_code == "housing_value_avg_dollar") {
        sus_translate("Property values in the area are higher than average ",
                      "for {island_or_region}.")
      } else {
        sus_translate("The area's percentage of tenants is higher than ",
                      "average for {island_or_region}.")
      }
    } else NULL
    
  } else if (theme == "Transport") {
    z <- out[out$var_code %in% "trans_car_pct", ]
    z <- z[z$percentile == max(z$percentile), ]
    
    more_less <- if (z$percentile > 0.5) sus_translate("more") else 
      sus_translate("less")
    
    sus_translate("Residents in the area drive to work {more_less} than ",
                  "average for {island_or_region}.")
  } else NULL
  
}
