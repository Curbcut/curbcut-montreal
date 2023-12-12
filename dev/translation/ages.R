# Go over every possible variable
vars <- variables$var_code[grepl("^age_agg_", variables$var_code)]

translation_age <- tibble::tibble(en = "Aged 85 and above (%)",
                                  fr = "Agés de plus de 85 ans (%)") |> 
  tibble::add_row(en = "are aged 85 and above",
                  fr = "sont âgés de plus de 85 ans")

additional_vars <- lapply(vars, \(var) {
  
  var_title <- (\(x) {
    if (var == "age_agg_85plus") return("Agés de plus de 85 ans (%)")
    start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
    end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
    if (end == "85plus") end <- "plus de 85"
    
    
    sprintf("Agés entre %s et %s ans (%%)", start, end)
  })()
  
  var_short <- (\(x) {
    if (var == "age_agg_85plus") return("85+ yo")
    start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
    end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
    if (end == "85plus") end <- "85+"
    
    
    sprintf("%s-%s ans", start, end)
  })()
  
  explanation <- (\(x) {
    if (var == "age_agg_85plus") 
      return("le pourcentage de la population âgée de plus de 85 ans")
    start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
    end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
    if (end == "85plus") end <- "plus de 85"
    
    
    sprintf("le pourcentage de la population âgée de %s à %s ans", start, end)
  })()
  
  explanation_nodet <- gsub("le ", "", explanation)
  
  exp_q5 <- (\(x) {
    if (var == "age_agg_85plus") 
      return("sont âgés de plus de 85 ans")
    start <- stringr::str_extract(var, "(?<=age_agg_).*(?=_)")
    end <- stringr::str_extract(var, "(?<=age_agg_\\d{1,2}_).*")
    if (end == "85plus") end <- "plus de 85"
    
    
    sprintf("sont âgés de %s à %s ans", start, end)
  })()
  
  
  # Construct the table
  tibble(en = variables$var_title[variables$var_code == var],
         fr = var_title) |> 
    add_row(en = variables$var_short[variables$var_code == var],
            fr = var_short) |> 
    add_row(en = variables$explanation[variables$var_code == var],
            fr = explanation) |> 
    add_row(en = variables$exp_q5[variables$var_code == var],
            fr = exp_q5) |> 
    add_row(en = variables$explanation_nodet[variables$var_code == var],
            fr = explanation_nodet)
  
}) |> (\(x) Reduce(rbind, x))()


translation_age <- rbind(translation_age, additional_vars)
