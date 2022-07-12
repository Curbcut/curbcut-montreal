#### GET PLACE EXPLORER BLOCK ##################################################

get_pe_block <- function(r = r, df, theme, select_id, island_or_region) {
  
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
      sus_translate(r = r, "Top {per}")
    } else {
      per <- scales::percent(abs(percentiles))
      if (per == "0%") per <- "1%"
      sus_translate(r = r, "Bottom {per}")
    }
  })
  
  # Pretty grid output
  block_text$value <- mapply(convert_unit, block_text$value, 
                             block_text$var_code, USE.NAMES = FALSE)
  
  block_text <- block_text[c("var_title", "explanation", "percentile", "value")]
  
  
  ## Get block plots -----------------------------------------------------------

  # Plot
  plots <- lapply(names(data_var), \(var) {
    
    quantile <- 
      round(data_sel[[var]][, paste0(island_or_region, "_percentile")]*100/5)*5
    
    filename <- paste0(paste0("www/place_explorer/", df, "_"),
                       paste(island_or_region, var, quantile, 
                             sep = "_"),
                       ".png")

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Plot"),
         width = "100%",
         height = "100%")
    
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
  ior <- sus_translate(r = r, "the ", island_or_region)
  
  # Age
  sentence <- if (theme == "Age") {
    
    z <- max(out$percentile)
    z_var <- out$var_code[out$percentile == z]
    age <- if (z_var == "age_0_14_pct") {
      "0-14"
    } else if (z_var == "age_15_64_pct") {
      "15-64"
    } else if (z_var == "age_65_plus_pct") {
      "65+"
    }
    sus_translate(r = r, "The area's residents are disproportionately in the {age} ",
                  "age range, compared to the rest of {ior}.")

  # Climate risk  
  } else if (theme == "Climate risk") {
    
    z <- mean(out$percentile)
    more_less <- if (z >= 0.8) {
      sus_translate(r = r, "much higher")
    } else if (z >= 0.6) {
      sus_translate(r = r, "higher")
    } else if (z >= 0.5) {
      sus_translate(r = r, "slightly higher")
    } else if (z >= 0.4) {
      sus_translate(r = r, "slightly lower")
    } else if (z >= 0.2) {
      sus_translate(r = r, "lower")
    } else sus_translate(r = r, "much lower")
    sus_translate(r = r, "The area has a {more_less} level of climate risk than ",
                  "average for {ior}.")
    
  # Education
  } else if (theme == "Education") {
    
    z <- out$percentile[out$var_code == "edu_bachelor_above_pct"]
    more_less <- if (z >= 0.8) {
      sus_translate(r = r, "much more")
    } else if (z >= 0.6) {
      sus_translate(r = r, "more")
    } else if (z >= 0.5) {
      sus_translate(r = r, "slightly more")
    } else if (z >= 0.4) {
      sus_translate(r = r, "slightly less")
    } else if (z >= 0.2) {
      sus_translate(r = r, "less")
    } else sus_translate(r = r, "much less")
    sus_translate(r = r, "Residents of the area are {more_less} likely than the rest ",
                  "of {ior} to have a university degree.")

  # Employment
  } else if (theme == "Employment") {
    
    z <- mean(out$percentile)
    more_less <- if (z >= 0.8) {
      sus_translate(r = r, "much higher")
    } else if (z >= 0.6) {
      sus_translate(r = r, "higher")
    } else if (z >= 0.5) {
      sus_translate(r = r, "slightly higher")
    } else if (z >= 0.4) {
      sus_translate(r = r, "slightly lower")
    } else if (z >= 0.2) {
      sus_translate(r = r, "lower")
    } else sus_translate(r = r, "much lower")
    sus_translate(r = r, "A {more_less} than average share of the area's residents ",
                  "work in creative and professional occupations compared to ",
                  "the rest of {ior}.")
    
  # Household
  } else if (theme == "Household") {
    
    z <- mean(c(1 - out$percentile[out$var_code == "family_one_person_pct"],
                out$percentile[out$var_code == "family_children_pct"]))
    more_less <- if (z >= 0.8) {
      sus_translate(r = r, "much larger")
    } else if (z >= 0.6) {
      sus_translate(r = r, "larger")
    } else if (z >= 0.5) {
      sus_translate(r = r, "slightly larger")
    } else if (z >= 0.4) {
      sus_translate(r = r, "slightly smaller")
    } else if (z >= 0.2) {
      sus_translate(r = r, "smaller")
    } else sus_translate(r = r, "much smaller")
    sus_translate(r = r, "The area's families are {more_less} than average for {ior}.")
    
  # Housing
  } else if (theme == "Housing") {
    
    z <- out$percentile[out$var_code == "housing_rent_avg_dollar"] *
      out$value[out$var_code == "housing_tenant_pct"] +
      out$percentile[out$var_code == "housing_value_avg_dollar"] *
      (1 - out$percentile[out$var_code == "housing_tenant_pct"])
    more_less <- if (z >= 0.8) {
      sus_translate(r = r, "much more expensive")
    } else if (z >= 0.6) {
      sus_translate(r = r, "more expensive")
    } else if (z >= 0.5) {
      sus_translate(r = r, "slightly more expensive")
    } else if (z >= 0.4) {
      sus_translate(r = r, "slightly cheaper")
    } else if (z >= 0.2) {
      sus_translate(r = r, "cheaper")
    } else sus_translate(r = r, "much cheaper")
    sus_translate(r = r, "Housing costs in the area are {more_less} than average ",
                  "for {ior}.")

  # Identity
  } else if (theme == "Identity") {
    
    z <- out$percentile[out$var_code == "iden_imm_pct"]
    more_less <- if (z >= 0.8) {
      sus_translate(r = r,  "much more")
    } else if (z >= 0.6) {
      sus_translate(r = r, "more")
    } else if (z >= 0.5) {
      sus_translate(r = r, "slightly more")
    } else if (z >= 0.4) {
      sus_translate(r = r, "slightly fewer")
    } else if (z >= 0.2) {
      sus_translate(r = r, "fewer")
    } else sus_translate(r = r, "much fewer")
    sus_translate(r = r, "The area has {more_less} foreign-born residents than ",
                  "average for {ior}.")
    
  # Income
  } else if (theme == "Income") {
    
    z <- out$percentile[out$var_code == "inc_median_dollar"]
    more_less <- if (z >= 0.8) {
      sus_translate(r = r, "much higher")
    } else if (z >= 0.6) {
      sus_translate(r = r, "higher")
    } else if (z >= 0.5) {
      sus_translate(r = r, "slightly higher")
    } else if (z >= 0.4) {
      sus_translate(r = r, "slightly lower")
    } else if (z >= 0.2) {
      sus_translate(r = r, "lower")
    } else sus_translate(r = r, "much lower")
    sus_translate(r = r, "Incomes in the area are {more_less} than average for {ior}.")

  # Language
  } else if (theme == "Language") {
    
    z_table <- out[out$percentile == max(out$percentile), ]
    z <- z_table$percentile
    z_var <- z_table$var_code
    more_less <- if (z >= 0.8) {
      sus_translate(r = r, "much more")
    } else if (z >= 0.6) {
      sus_translate(r = r, "more")
    } else if (z >= 0.5) {
      sus_translate(r = r, "slightly more")
    } else if (z >= 0.4) {
      sus_translate(r = r, "slightly less")
    } else if (z >= 0.2) {
      sus_translate(r = r, "less")
    } else sus_translate(r = r, "much less")
    lang <- if (z_var == "lang_eng_only_pct") {
      sus_translate(r = r, "speak English")
    } else if (z_var == "lang_french_only_pct") {
      sus_translate(r = r, "speak French")
    } else if (z_var == "lang_french_eng_pct") {
      sus_translate(r = r, "be bilingual (French and English)")
    } else if (z_var == "lang_no_official_pct") {
      sus_translate(r = r, "speak neither French nor English")
    }
    sus_translate(r = r, "The area's residents are {more_less} likely to {lang} ",
                  "than average for {ior}.")

  # Transport
  } else if (theme == "Transport") {
    
    z <- out$percentile[out$var_code == "trans_car_pct"]

    if (length(z) > 0) {
      more_less <- if (z >= 0.8) {
        sus_translate(r = r, "much more")
      } else if (z >= 0.6) {
        sus_translate(r = r, "more")
      } else if (z >= 0.5) {
        sus_translate(r = r, "slightly more")
      } else if (z >= 0.4) {
        sus_translate(r = r, "slightly less")
      } else if (z >= 0.2) {
        sus_translate(r = r, "less")
      } else sus_translate(r = r, "much less")
      
      sus_translate(r = r, "Residents in the area drive to work {more_less} than ",
                    "average compared to the rest of {ior}.")
    } else {
      z <- out$percentile[out$var_code == "access_jobs_total"]
      more_less <- if (z >= 0.8) {
        sus_translate(r = r, "much more")
      } else if (z >= 0.6) {
        sus_translate(r = r, "more")
      } else if (z >= 0.5) {
        sus_translate(r = r, "slightly more")
      } else if (z >= 0.4) {
        sus_translate(r = r, "slightly less")
      } else if (z >= 0.2) {
        sus_translate(r = r, "less")
      } else sus_translate(r = r, "much less")
      
      sus_translate(r = r, "The area has {more_less} public transit access to jobs ",
                    "than the rest of {ior}.")
      
    }
  } else NULL
  
  
  ## Combine and return output -------------------------------------------------
  
  return(list(block_text, plots, sentence))
  
}
