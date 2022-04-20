#### NATURAL INFRASTRUCTURE EXPLORE INFO TABLE GRAPH ##########################

info_table_natural_inf <- function(data, var_type, var_left, 
                                              var_right, df, select_id, 
                                              build_str_as_DA = TRUE) {
  
  conservation_pct <- data$conservation_pct 
  slider <- data$slider
  
  flood <- data$flood |> scales::percent()
  biodiversity <- data$biodiversity |> scales::percent()
  heat_island <- data$heat_island |> scales::percent()
  
  HTML(
    sus_translate("<p>Natural infrastructure represents approximately 25% of ", 
                  "the territory of the Montreal region. Preserving ",
                  "{slider}% of the overall territory as natural ",
                  "infrastructure means that {conservation_pct}% of the ",
                  "natural infrastructure of the region would be protected.",
                  "</p><p>This level of protection allows for the ",
                  "conservation of {flood} of the runoff reduction, ",
                  "{biodiversity} of the biodiversity conservation, and ",
                  "{heat_island} of the urban heat island reduction effects ",
                  "provided by natural infrastructure in the region.</p>")
  )
}
