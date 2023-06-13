## TENURE VARIABLES TRANSLATION ################################################

translation_tenure <- 
  tibble(en = character(),
         fr = character()) |> 
  add_row(en = "Number of households",
          fr = "Nombre de ménages") |> 
  add_row(en = "Percent of all households",
          fr = "Pourcentage de tous les ménages") |> 
  add_row(en = "Suitability and condition",
          fr = "Adéquation et état") |> 
  add_row(en = "Suitable accommodations",
          fr = "Logement de taille suffisante") |> 
  add_row(en = "Unsuitable accommodations",
          fr = "Logement de taille insuffisante") |> 
  add_row(en = "Dwellings needing minor repairs",
          fr = "Logements nécessitant des réparations mineures") |> 
  add_row(en = "Dwellings needing major repairs",
          fr = "Logements nécessitant des réparations majeures") |> 
  add_row(en = "Family composition",
          fr = "Composition de la famille") |> 
  add_row(en = "Census families in private households",
          fr = "Familles de recensement dans des ménages privés") |> 
  add_row(en = "One-parent",
          fr = "Familles monoparentales") |> 
  add_row(en = "One-parent families in which the parent is a man+",
          fr = "Familles monoparentales dont le parent est un homme+") |> 
  add_row(en = "One-parent families in which the parent is a woman+",
          fr = "Familles monoparentales dont le parent est une femme+") |> 
  add_row(en = "Couple families",
          fr = "Couples") |> 
  add_row(en = "Families without children",
          fr = "Familles sans enfants") |> 
  add_row(en = "Families with children",
          fr = "Familles avec enfants") |> 
  add_row(en = "Families with 1 or 2 children",
          fr = "Families avec 1 ou 2 enfants") |> 
  add_row(en = "Families with 3 or more children",
          fr = "Familles avec 3 enfants ou plus") |> 
  add_row(en = "Living alone",
          fr = "Personnes vivant seules") |> 
  add_row(en = "Living alone and aged between 18 and 64 years old",
          fr = "Personnes vivant seules et âgées entre 18 et 64 ans") |> 
  add_row(en = "Living alone and aged 65 years old and over",
          fr = "Personnes vivant seules et âgées de 65 ans et plus")


# Go over every possible variable
vars <- variables$var_code[grepl("^tenure_", variables$var_code)]

additional_vars <- lapply(vars, \(var) {
  
  
  # All characteristics
  characteristics <- (\(x) {
    out <- list()
    
    if (grepl("_owner_", var)) out <- c(out, "sont propriétaires")
    if (grepl("_tenant_", var)) out <- c(out, "sont locataires")
    
    if (grepl("_sc30_", var)) out <- c(out, "dépensent plus de 30 % de leurs revenus pour se loger")
    if (grepl("_sc50_", var)) out <- c(out, "dépensent plus de 50 % de leurs revenus pour se loger")
    if (grepl("_sc80_", var)) out <- c(out, "dépensent plus de 80 % de leurs revenus pour se loger")
    
    if (grepl("_suit_", var)) out <- c(out, "vivent dans un logement de taille convenable (suffisamment de chambres à coucher pour la taille et la composition des ménages)")
    if (grepl("_unsuit_", var)) out <- c(out, "vivent dans des logements de taille insuffisante (pas assez de chambres pour la taille et la composition des ménages)")
    if (grepl("_minorrep_", var)) out <- c(out, "vivent dans des logements nécessitant des réparations mineures (carreaux de plancher détachés ou manquants, des briques descellées, des bardeaux arrachés; ou des marches, des rampes ou un revêtement extérieur défectueux)")
    if (grepl("_majorrep_", var)) out <- c(out, "vivent dans des logements nécessitant des réparations majeures (plomberie ou installation électrique est défectueuse; besoin de réparations à la charpente des murs, planchers ou plafonds)")
    
    if (grepl("_family_", var)) out <- c(out, "sont des familles de recensement dans des ménages privés")
    if (grepl("_monop_", var)) out <- c(out, "sont des familles monoparentales")
    if (grepl("_monopmen_", var)) out <- c(out, "sont des familles monoparentales dont le parent est un homme+")
    if (grepl("_monopwomen_", var)) out <- c(out, "sont des familles monoparentales dont le parent est une femme+")
    if (grepl("_couple_", var)) out <- c(out, "sont des couples")
    if (grepl("_nochildren_", var)) out <- c(out, "sont des familles sans enfants")
    if (grepl("_withchildren_", var)) out <- c(out, "sont des familles avec enfants")
    if (grepl("_12children_", var)) out <- c(out, "sont des familles avec 1 ou 2 enfants")
    if (grepl("_3pluschildren_", var)) out <- c(out, "sont des familles avec plus de 3 enfants")
    if (grepl("_livingalone_", var)) out <- c(out, "vivent seuls")
    if (grepl("_la1864_", var)) out <- c(out, "vivent seuls et sont âgés de 18 à 64 ans")
    if (grepl("_la65_", var)) out <- c(out, "vivent seuls et sont âgés de 65 ans et plus")
    
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
    
    if (grepl("_suit_", var)) out <- c(out, "Logement de taille convenable")
    if (grepl("_unsuit_", var)) out <- c(out, "Logements de taille insuffisante")
    if (grepl("_minorrep_", var)) out <- c(out, "Logements nécessitant des réparations mineures")
    if (grepl("_majorrep_", var)) out <- c(out, "Logements nécessitant des réparations majeures")
    
    if (grepl("_family_", var)) out <- c(out, "Familles de recensement dans des ménages privés")
    if (grepl("_monop_", var)) out <- c(out, "Familles monoparentales")
    if (grepl("_monopmen_", var)) out <- c(out, "Familles monoparentales dont le parent est un homme+")
    if (grepl("_monopwomen_", var)) out <- c(out, "Familles monoparentales dont le parent est une femme+")
    if (grepl("_couple_", var)) out <- c(out, "Couples")
    if (grepl("_nochildren_", var)) out <- c(out, "Familles sans enfants")
    if (grepl("_withchildren_", var)) out <- c(out, "Familles avec enfants")
    if (grepl("_12children_", var)) out <- c(out, "Familles avec 1 ou 2 enfants")
    if (grepl("_3pluschildren_", var)) out <- c(out, "Familles avec plus de 3 enfants")
    if (grepl("_livingalone_", var)) out <- c(out, "Vivent seuls")
    if (grepl("_la1864_", var)) out <- c(out, "Vivent seuls et sont âgés de 18 à 64 ans")
    if (grepl("_la65_", var)) out <- c(out, "Vivent seuls et sont âgés de 65 ans et plus")
    
    if (length(out) == 0) return({
      if (grepl("_pct", var)) return("Tous les ménages")
      return("Ménages")
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
    
    if (grepl("_suit_", var)) out <- c(out, "Conv.")
    if (grepl("_unsuit_", var)) out <- c(out, "Non-conv.")
    if (grepl("_minorrep_", var)) out <- c(out, "Min.")
    if (grepl("_majorrep_", var)) out <- c(out, "Maj.")
    
    
    if (grepl("_family_", var)) out <- c(out, "Fam.")
    if (grepl("_monop_", var)) out <- c(out, "Monop.")
    if (grepl("_monopmen_", var)) out <- c(out, "Monop. M+.")
    if (grepl("_monopwomen_", var)) out <- c(out, "Monop. W+.")
    if (grepl("_couple_", var)) out <- c(out, "Couple.")
    if (grepl("_nochildren_", var)) out <- c(out, "Pas enf..")
    if (grepl("_withchildren_", var)) out <- c(out, "Avec enf.")
    if (grepl("_12children_", var)) out <- c(out, "1-2 enf.")
    if (grepl("_3pluschildren_", var)) out <- c(out, "3+ enf.")
    if (grepl("_livingalone_", var)) out <- c(out, "Seuls.")
    if (grepl("_la1864_", var)) out <- c(out, "Seuls 18-64.")
    if (grepl("_la65_", var)) out <- c(out, "Seuls 65+.")
    
    if (length(out) == 0) return({
      if (grepl("_pct", var)) return("Tous les mén.")
      return("Mén.")
    })
    paste0(out, collapse = " ")
  })()
  
  # Explanation
  explanation <- if (length(characteristics) > 1) {
    exp <- "le pourcentage de ménages qui présentent toutes les caractéristiques suivantes :<ul><li>"
    exp_c <- paste0(characteristics, collapse = "<li>")
    paste0(exp, exp_c, "</ul>") 
  } else if (length(characteristics) == 1) {
    paste0("le pourcentage de ménages qui ", characteristics[[1]])
  } else {
    "le pourcentage de tous les ménages"
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
    "habitent dans la région"
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

translation_tenure <- rbind(translation_tenure, additional_vars)
