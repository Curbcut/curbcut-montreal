## AFFORDABILITY VARIABLES TRANSLATION #########################################

translation_afford <- 
  tibble(en = character(),
         fr = character()) |> 
  add_row(en = "Number of individuals",
          fr = "Nombre d'individus") |> 
  add_row(en = "Percent of all individuals",
          fr = "Pourcentage de tous les individus") |> 
  add_row(en = "Owners",
          fr = "Propriétaires") |> 
  add_row(en = "Tenants",
          fr = "Locataires") |> 
  add_row(en = "Tenure status",
          fr = "Statut d'occupation") |> 
  add_row(en = "Shelter cost to income ratio",
          fr = "Ratio coût du logement/revenu") |> 
  add_row(en = ">30%",
          fr = ">30%") |> 
  add_row(en = ">50%",
          fr = ">50%") |> 
  add_row(en = ">80%",
          fr = ">80%") |> 
  add_row(en = ">0%",
          fr = ">0%") |> 
  add_row(en = "Immigration status",
          fr = "Statut d'immigration") |> 
  add_row(en = "All immigrants",
          fr = "Tous les immigrations") |> 
  add_row(en = "Not immigrants",
          fr = "Non-immigrants") |> 
  add_row(en = "Refugees",
          fr = "Réfugiés") |> 
  add_row(en = "Non-permanent residents",
          fr = "Résidents non permanents") |> 
  add_row(en = "Immigrated more than 5 years ago",
          fr = "Immigré depuis plus de 5 ans") |> 
  add_row(en = "Immigrated in the past 5 years",
          fr = "Immigré au cours des 5 dernières années") |> 
  add_row(en = "Gender",
          fr = "Genre") |> 
  add_row(en = "Men+",
          fr = "Hommes+") |> 
  add_row(en = "Women+",
          fr = "Femmes+") |> 
  add_row(en = "Poverty status",
          fr = "Situation de pauvreté") |> 
  add_row(en = "In a situation of poverty",
          fr = "En situation de pauvreté") |> 
  add_row(en = "Not in a situation of poverty",
          fr = "Pas en situation de pauvreté") |> 
  add_row(en = "Unit of analysis",
          fr = "Unité d'analyse")


# Go over every possible variable
vars <- variables$var_code[grepl("^affordpop_", variables$var_code)]

additional_vars <- lapply(vars, \(var) {
  
  
  # All characteristics
  characteristics <- (\(x) {
    out <- list()
    
    if (grepl("_owner_", var)) out <- c(out, "sont propriétaires")
    if (grepl("_tenant_", var)) out <- c(out, "sont locataires")
    
    if (grepl("_sc30_", var)) out <- c(out, "dépensent plus de 30 % de leurs revenus pour se loger")
    if (grepl("_sc50_", var)) out <- c(out, "dépensent plus de 50 % de leurs revenus pour se loger")
    if (grepl("_sc80_", var)) out <- c(out, "dépensent plus de 80 % de leurs revenus pour se loger")
    
    if (grepl("_imm_", var)) out <- c(out, "sont immigrants")
    if (grepl("_nimm_", var)) out <- c(out, "ne sont pas immigrants")
    if (grepl("_ref_", var)) out <- c(out, "sont des réfugiés")
    if (grepl("_nper_", var)) out <- c(out, "sont des résidents non permanents")
    if (grepl("_nrecentimm_", var)) out <- c(out, "ont immigré il y a plus de 5 ans")
    if (grepl("_recentimm_", var)) out <- c(out, "ont immigré au cours des 5 dernières années")
    
    if (grepl("_men_", var)) out <- c(out, "sont des hommes+")
    if (grepl("_women_", var)) out <- c(out, "sont des femmes+")
    
    if (grepl("_poverty_", var)) out <- c(out, "sont en situation de pauvreté")
    if (grepl("_nopoverty_", var)) out <- c(out, "ne sont pas en situation de pauvreté")
    
    return(out)
  })()
  
  # Titles
  title <- (\(x) {
    out <- list()
    
    if (grepl("_owner_", var)) out <- c(out, "Propriétaires")
    if (grepl("_tenant_", var)) out <- c(out, "Locataires")
    
    if (grepl("_sc30_", var)) out <- c(out, "Dépense > 30 % du revenu pour le logement")
    if (grepl("_sc50_", var)) out <- c(out, "Dépense > 50 % du revenu pour le logement")
    if (grepl("_sc80_", var)) out <- c(out, "Dépense > 80 % du revenu pour le logement")
    
    if (grepl("_imm_", var)) out <- c(out, "Immigrants")
    if (grepl("_nimm_", var)) out <- c(out, "Non immigrants")
    if (grepl("_ref_", var)) out <- c(out, "Réfugiés")
    if (grepl("_nper_", var)) out <- c(out, "Résidents non permanents")
    if (grepl("_nrecentimm_", var)) out <- c(out, "Immigré depuis plus de 5 ans")
    if (grepl("_recentimm_", var)) out <- c(out, "Immigré au cours des 5 dernières années")
    
    if (grepl("_men_", var)) out <- c(out, "Hommes+")
    if (grepl("_women_", var)) out <- c(out, "Femmes+")
    
    if (grepl("_poverty_", var)) out <- c(out, "En situation de pauvreté")
    if (grepl("_nopoverty_", var)) out <- c(out, "Pas en situation de pauvreté")
    
    if (length(out) == 0) return({
      if (grepl("_pct", var)) return("Tous les individus")
      return("Individus")
    })
    stringr::str_to_sentence(paste0(out, collapse = ", "))
  })()
  
  
  short_title <- (\(x) {
    out <- list()
    
    if (grepl("_owner_", var)) out <- c(out, "Prop.")
    if (grepl("_tenant_", var)) out <- c(out, "Loc.")
    
    if (grepl("_sc30_", var)) out <- c(out, ">30%.")
    if (grepl("_sc50_", var)) out <- c(out, ">50%.")
    if (grepl("_sc80_", var)) out <- c(out, ">80%.")
    
    if (grepl("_imm_", var)) out <- c(out, "Imm.")
    if (grepl("_nimm_", var)) out <- c(out, "Non imm.")
    if (grepl("_ref_", var)) out <- c(out, "Réf.")
    if (grepl("_nper_", var)) out <- c(out, "Non perm.")
    if (grepl("_nrecentimm_", var)) out <- c(out, "Imm >5.")
    if (grepl("_recentimm_", var)) out <- c(out, "Imm <5.")
    
    if (grepl("_men_", var)) out <- c(out, "H.+")
    if (grepl("_women_", var)) out <- c(out, "F.+")
    
    if (grepl("_poverty_", var)) out <- c(out, "Pauv.")
    if (grepl("_nopoverty_", var)) out <- c(out, "Non pauv.")
    
    if (length(out) == 0) return({
      if (grepl("_pct", var)) return("Tous ind.")
      return("Ind.")
    })
    paste0(out, collapse = " ")
  })()
  
  # Explanation
  explanation <- if (length(characteristics) > 1) {
    exp <- "le pourcentage d'individus qui présentent toutes les caractéristiques suivantes :<ul><li>"
    exp_c <- paste0(characteristics, collapse = "<li>")
    paste0(exp, exp_c, "</ul>") 
  } else if (length(characteristics) == 1) {
    paste0("le pourcentage d'individus qui ", characteristics[[1]])
  } else {
    "le pourcentage de tous les individus"
  }
  if (grepl("_count$", var)) explanation <- gsub(" pourcentage ", " nombre ", explanation)
  explanation_nodet <- gsub("^le ", "", explanation)
  
  
  # Explanation (for q5)
  exp_q5 <- if (length(characteristics) > 1) {
    exp <- "présentent toutes les caractéristiques suivantes:<ul><li>"
    exp_c <- paste0(characteristics, collapse = "<li>")
    paste0(exp, exp_c, "</ul>") 
  } else if (length(characteristics) == 1) {
    characteristics[[1]]
  } else {
    "vivent dans la région"
  }
  if (grepl("_count$", var)) exp_q5 <- gsub(" pourcentage ", " nombre ", exp_q5)
  
  # Construct the table
  tibble(en = variables$var_title[variables$var_code == var],
         fr = title) |> 
    add_row(en = variables$var_short[variables$var_code == var],
            fr = short_title) |> 
    add_row(en = variables$explanation[variables$var_code == var],
            fr = explanation) |> 
    add_row(en = variables$exp_q5[variables$var_code == var],
            fr = exp_q5) |> 
    add_row(en = variables$explanation_nodet[variables$var_code == var],
            fr = explanation_nodet)
  
}) |> (\(x) Reduce(rbind, x))()

translation_afford <- rbind(translation_afford, additional_vars)
