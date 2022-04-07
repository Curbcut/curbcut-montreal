#### PLACE EXPLORER FUNCTIONS ##################################################

# Grid output
place_explorer_block_text <- function(df, theme, select_id,
                                      island_or_region) {

  raw_data_order <- pe_variable_order[[df]]
  data_order <- raw_data_order[raw_data_order$group == island_or_region, ]
  data_order <- data_order[data_order$theme == theme &
                             data_order$ID == select_id, c("var_code")]
  
  if (nrow(data_order) == 0) return(data.frame())

  if (df == "CT" && theme == "Transport")
    data_order <- unique.data.frame(data_order)

  # Access for CT
  variables_var_codes <-
    if (df == "CT" && theme == "Transport") {
      rbind(
        variables[!grepl("access", variables$var_code), ], {
          access_vars <- variables[grepl("access", variables$var_code), ]

          new_var_code <- c(
          access_vars$var_code[str_starts(access_vars$var_code, "access_jobs")] |> 
            str_extract("access_jobs_[^_]*"),
          access_vars$var_code[!str_starts(access_vars$var_code, "access_jobs")] |> 
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
            str_replace(access_vars$explanation, paste0(exp_suffix, collapse = "|"),
                        "on average")

          access_vars
        }
      )
    } else variables

  variables_theme <- variables_var_codes[variables_var_codes$var_code %in% data_order$var_code, ]
  variables_theme <-
    variables_theme[order(match(variables_theme$var_code, data_order$var_code)),
                    c("var_title", "explanation")]
  data_order <- cbind(data_order, variables_theme)

  raw_data_var <- pe_var_hierarchy[[df]][names(pe_var_hierarchy[[df]]) %in%
                                           data_order$var_code]
  raw_data_var <- raw_data_var[order(match(names(raw_data_var), data_order$var_code))]
  data_var <- lapply(raw_data_var, \(x) {x[x$ID == select_id, ]})

  col <- paste0(island_or_region, "_percentile")
  data_var <- 
    data.frame(
      col = sapply(raw_data_var, \(x) x[x$ID == select_id,][[col]], 
                   USE.NAMES = FALSE),
      var_code = names(raw_data_var),
      value = sapply(raw_data_var, \(x) x$var[x$ID == select_id], 
                     USE.NAMES = FALSE),
      row.names = NULL
    ) |> 
    setNames(c(col, "var_code", "value"))
  
  names(data_var)[1] <- "percentile"

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
  out$value <- mapply(convert_unit, out$value, out$var_code, 
                      USE.NAMES = FALSE)

  return(out[, c("var_title", "explanation", "percentile", "value")])

}

# Plots for the grid output
place_explorer_block_plot <- function(df, theme, select_id,
                                      island_or_region) {
  
  raw_data_order <- pe_variable_order[[df]]
  data_order <- raw_data_order[raw_data_order$group == island_or_region, ]
  data_order <- data_order[data_order$theme == theme &
                             data_order$ID == select_id,]["var_code"]
  
  if (df == "CT" && theme == "Transport")
    data_order <- unique.data.frame(data_order)
  
  raw_data_var <- pe_var_hierarchy[[df]][
    names(pe_var_hierarchy[[df]]) %in% data_order$var_code]
  
  # Plot
  plots <- lapply(data_order$var_code, \(var_code) {
    
    hex_to_plot <- "#A9A9A9"
    
    data <- raw_data_var[[var_code]]
    data <- data[!is.na(data$var), ]
    data_var <- data[data$ID == select_id, ]$var
    var_outlier <- remove_outliers(data$var)
    outlier <- if (data_var %in% var_outlier) FALSE else TRUE
    
    if (!is.na(data_var)) {
      data_out <- if (outlier) data[data$var %in% var_outlier,] else data
      ggplot(data_out) +
        geom_density(aes(x = var), size = 1, color = hex_to_plot) +
        geom_vline(aes(xintercept = data_var), color = "#000000", size = 1,
                   alpha = 1) +
        theme_void()
    } else ggplot()
    
  })
  
}

# Small introduction sentence to the block
place_explorer_block_sentence <- function(df, theme, select_id,
                                      island_or_region) {
  
  raw_data_order <- pe_variable_order[[df]]
  data_order <- raw_data_order[raw_data_order$group == island_or_region, ]
  data_order <- data_order[data_order$theme == theme &
                             data_order$ID == select_id, c("var_code")]
  
  variables_theme <- variables[variables$var_code %in% data_order$var_code, ]
  variables_theme <-
    variables_theme[order(match(variables_theme$var_code, data_order$var_code)),
                    c("var_title", "explanation")]
  data_order <- cbind(data_order, variables_theme)
  
  raw_data_var <- pe_var_hierarchy[[df]][names(pe_var_hierarchy[[df]]) %in%
                                           data_order$var_code]
  raw_data_var <- raw_data_var[order(match(names(raw_data_var), data_order$var_code))]
  data_var <- lapply(raw_data_var, \(x) {x[x$ID == select_id, ]})
  
  col <- paste0(island_or_region, "_percentile")
  data_var <- 
    data.frame(
      col = sapply(raw_data_var, \(x) x[x$ID == select_id,][[col]], 
                   USE.NAMES = FALSE),
      var_code = names(raw_data_var),
      value = sapply(raw_data_var, \(x) x$var[x$ID == select_id], 
                     USE.NAMES = FALSE),
      row.names = NULL
    ) |> 
    setNames(c(col, "var_code", "value"))
  
  names(data_var)[1] <- "percentile"
  
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
