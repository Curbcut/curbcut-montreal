#### NATURAL INFRASTRUCTURE EXPLORE INFO TABLE GRAPH ##########################

info_table_natural_inf <- function(data, r = r, var_type, var_left, 
                                   var_right, df, select_id, geo,
                                   build_str_as_DA = TRUE) {
  
  if (var_left == "c_priority") {
    conservation_pct <- data$conservation_pct 
    slider <- data$slider
    
    flood <- data$c_flood |> scales::percent()
    biodiversity <- data$c_biodiversity |> scales::percent()
    heat_island <- data$c_heat_island |> scales::percent()
    
    HTML(cc_t(r = r, 
      "<p>Natural infrastructure represents approximately 25% of the ",
      "territory of the Montreal region. Preserving {slider}% of the overall ",
      "territory as natural infrastructure means that {conservation_pct}% ",
      "of the natural infrastructure of the region would be protected.</p>",
      "<p>This level of protection allows for the ",
      "conservation of {flood} of the runoff reduction, ",
      "{biodiversity} of the biodiversity conservation, and ",
      "{heat_island} of the urban heat island reduction effects ",
      "provided by natural infrastructure in the region.</p>"))
    
  } else if (var_left %in% c("habitat_qual", "habitat_con", "favorable_cc",
                             "c_bio")) {
    c_bio <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'c_bio']), ":</i> ", 
      cc_t(r = r, variables$explanation[variables$var_code == 'c_bio']))
    habitat_qual <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'habitat_qual']), 
      ":</i> ", cc_t(r = r, variables$explanation[variables$var_code == 'habitat_qual']))
    habitat_con <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'habitat_con']), 
      ":</i> ", cc_t(r = r, variables$explanation[variables$var_code == 'habitat_con']))
    favorable_cc <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'favorable_cc']), 
      ":</i> ", cc_t(r = r, variables$explanation[variables$var_code == 'favorable_cc']))
    
    HTML(cc_t(r = r, 
      "<p>Natural infrastructure represents approximately 25% of the ",
      "territory of the Montreal region. Biodiversity-related natural ", 
      "infrastructure functions include:</p><ul>{c_bio}{habitat_qual}",
      "{habitat_con}{favorable_cc}</ul>"))
    
  } else if (var_left %in% c("c_flood", "flood")) {
    c_flood <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'c_flood']), ":</i> ", 
      cc_t(r = r, variables$explanation[variables$var_code == 'c_flood']))
    flood <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'flood']), ":</i> ", 
      cc_t(r = r, variables$explanation[variables$var_code == 'flood']))
    
    HTML(cc_t(r = r, 
      "<p>Natural infrastructure represents approximately 25% of the ",
      "territory of the Montreal region. Flood-related natural ", 
      "infrastructure functions include:</p><ul>{c_flood}{flood}</ul>"))
    
  } else if (var_left %in% c("c_heat", "heat", "cool")) {
    c_heat <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'c_heat']), ":</i> ", 
      cc_t(r = r, variables$explanation[variables$var_code == 'c_heat']))
    heat <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'heat']), ":</i> ", 
      cc_t(r = r, variables$explanation[variables$var_code == 'heat']))
    cool <- paste0(
      "<li><i>", cc_t(r = r, variables$var_title[variables$var_code == 'cool']), ":</i> ", 
      cc_t(r = r, variables$explanation[variables$var_code == 'cool']))
    
    HTML(cc_t(r = r, 
      "<p>Natural infrastructure represents approximately 25% of the ",
      "territory of the Montreal region. Heat-related natural ", 
      "infrastructure functions include:</p><ul>{c_heat}{heat}{cool}</ul>"))
    
  }
}
