# Go over every possible variable
vars <- variables$var_code[grepl("^age_agg_", variables$var_code)]

translation_age <- tibble::tibble(en = "Aged 85 and above (%)",
                                  fr = "Agés de plus de 85 ans (%)") |> 
  tibble::add_row(en = "are aged 85 and above",
                  fr = "sont âgés de plus de 85 ans") |> 
  tibble::add_row(en = "the percentage of the population aged 85 and above",
                  fr = "le pourcentage de la population âgée de plus de 85 ans") |> 
  tibble::add_row(en = "percentage of the population aged 85 and above",
                  fr = "pourcentage de la population âgée de plus de 85 ans")

additional_vars <- lapply(vars, \(u_var) {
  
  pct <- grepl("_pct$", u_var)
  var <- gsub("_pct|_count", "", u_var)  
  
  title <- (\(x) {
    if (grepl("age_agg_85plus", var)) {
      out <- "Agés de plus de 85 ans"
      if (pct) out <- paste(out, "(%)")
      return(out)
    }
    start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
    end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
    if (end == "85plus") end <- "plus de 85"
    
    
    out <- sprintf("Agés entre %s et %s ans", start, end)
    if (pct) out <- paste(out, "(%)")
    out
  })()
  
  short <- (\(x) {
    if (grepl("age_agg_85plus", var)) {
      out <- "85+ ans"
      if (pct) out <- paste(out, "(%)")
      return(out)
    }
    start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
    end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
    if (end == "85plus") end <- "85+"
    
    
    sprintf("%s-%s ans", start, end)
  })()
  
  explanation <- (\(x) {
    if (grepl("age_agg_85plus", var)) {
      out <- "de plus de 85 ans"
      
      beg <- if (pct) "pourcentage de la population âgée" else "nombre d'individus âgés"
      return(sprintf("le %s %s", beg, out))
    }

    start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
    end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
    if (end == "85plus") end <- "plus de 85"
    
    beg <- if (pct) "pourcentage de la population" else "nombre d'individus"
    sprintf("le %s âgée de %s à %s ans", beg, start, end)
  })()
  
  explanation_nodet <- gsub("le ", "", explanation)
  
  exp_q5 <- (\(x) {
    if (grepl("age_agg_85plus", var))
      return("sont âgés de plus de 85 ans")
    start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
    end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
    if (end == "85plus") end <- "plus de 85"
    
    
    sprintf("sont âgés de %s à %s ans", start, end)
  })()
  
  
  # Construct the table
  tibble(en = variables$var_title[variables$var_code == u_var],
         fr = title) |> 
    add_row(en = variables$var_short[variables$var_code == u_var],
            fr = short) |> 
    add_row(en = variables$explanation[variables$var_code == u_var],
            fr = explanation) |> 
    add_row(en = variables$exp_q5[variables$var_code == u_var],
            fr = exp_q5) |> 
    add_row(en = variables$explanation_nodet[variables$var_code == u_var],
            fr = explanation_nodet)
  
}) |> (\(x) Reduce(rbind, x))()


translation_age <- rbind(translation_age, additional_vars)
