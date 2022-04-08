#### GET PLACE EXPLORER BLOCK SENTENCE #########################################

get_pe_block_sentence <- function(df, theme, select_id, island_or_region,
                                  data_order) {
  
  variables_theme <- variables[variables$var_code %in% data_order$var_code, ]
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
  
  if (theme == "Age") {
    z <- out[out$var_code %in% c("age_65_plus_pct", "age_0_14_pct"), ]
    z <- z[z$percentile == max(z$percentile), ]
    if (z$percentile > 0.5) {
      older_younger <- 
        if (z$var_code == "age_65_plus_pct") sus_translate("older") else 
          sus_translate("younger")
      sus_translate("The population is {older_younger} than average.")
    }
  } else if (theme == "Identity") {
    z <- out[out$var_code == "iden_imm_pct", ]
    more_less <- if (z$percentile > 0.5) sus_translate("more") else 
      sus_translate("less")
    sus_translate("There are {more_less} foreign-born residents than average.")
    
  } else if (theme == "Income") {
    z <- out[out$var_code %in% c("inc_limat_pct", "inc_high_pct"), ]
    z <- z[z$percentile == max(z$percentile), ]
    
    if (z$percentile > 0.5) {
      higher_lower <- 
        if (z$var_code == "inc_high_pct") sus_translate("higher") else 
          sus_translate("lower")
      sus_translate("Income is {higher_lower} than average.")
    }
  } else if (theme == "Language") {
    z <- out[out$var_code %in% c("lang_eng_only_pct", "lang_french_only_pct",
                                 "lang_french_eng_pct"), ]
    z <- z[z$percentile == max(z$percentile), ]
    
    if (z$percentile > 0.5) {
      lang <- {
        if (z$var_code == "lang_eng_only_pct") {
          sus_translate("english")
        } else if (z$var_code == "lang_french_only_pct") {
          sus_translate("french")
        } else sus_translate("bilingual (french and english)")
      }
      sus_translate("The population speaks more {lang} than average.")
    }
  } else if (theme == "Education") {
    z <- out[out$percentile == max(out$percentile), ]
    
    if (z$percentile > 0.5) {
      more_less <- {
        if (z$var_code == "edu_bachelor_above_pct") sus_translate("more") else
          sus_translate("less")
      }
      sus_translate("The population has {more_less} university degrees than average.")
    }
  } else if (theme == "Housing") {
    z <- out[out$var_code %in% c("housing_tenant_pct", "housing_value_avg_dollar"), ]
    z <- z[z$percentile == max(z$percentile), ]
    
    if (z$percentile > 0.5) {
      if (z$var_code == "housing_value_avg_dollar") {
        sus_translate("Property values are above average.")
      } else {
        sus_translate("The percentage of tenants is higher than average.")
      }
    }
  } else if (theme == "Transport") {
    z <- out[out$var_code %in% "trans_car_pct", ]
    z <- z[z$percentile == max(z$percentile), ]
    
    more_less <- if (z$percentile > 0.5) sus_translate("more") else 
      sus_translate("less")
    
    sus_translate("The population drives {more_less} to work than average.")
  } else NULL
  
}
