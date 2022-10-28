### Place explorer translation #################################################

place_explorer_translated <- 
  tibble(en = character(), fr = character()) |>
  add_row(en = paste0("Island"), 
          fr = paste0("L'île")) |>
  add_row(en = paste0("Region"), 
          fr = paste0("Grande région")) |>
  add_row(en = paste0("The centraide zone"), 
          fr = paste0("La zone centraide")) |>
  add_row(en = paste0("What makes this area unique?"), 
          fr = paste0("Qu'est-ce qui rend cette zone unique ?")) |>
  add_row(en = paste0("What makes this area similar to others?"), 
          fr = paste0("Qu'est-ce qui rend cette zone semblable aux autres ?")) |>
  add_row(en = paste0("highest"), 
          fr = paste0("plus haut")) |>
  add_row(en = paste0("lowest"), 
          fr = paste0("plus bas")) |>
  add_row(en = paste0("The borough/city"), 
          fr = paste0("L'arrondissement/la ville")) |>
  add_row(en = paste0("The census tract"), 
          fr = paste0("Le secteur de recensement")) |>
  add_row(en = paste0("The dissemination area"), 
          fr = paste0("L'aire de diffusion")) |>
  add_row(en = paste0("Top {per}"), 
          fr = paste0("{per} plus haut")) |>
  add_row(en = paste0("{z$pretty_data_var} of residents use public transit, ",
                      "walk or bicycle to get to work. {z$data_rank}. (Data ",
                      "from {z$data_date})"), 
          fr = paste0("{z$pretty_data_var} des résidents utilisent les ",
                      "transports en commun, la marche ou le vélo pour ",
                      "se rendre au travail. {z$data_rank}. (Données de ",
                      "{z$data_date})")) |>
  add_row(en = paste0("Variable"), 
          fr = paste0("Variable")) |>
  add_row(en = paste0("Rank"), 
          fr = paste0("Rang")) |>
  add_row(en = paste0("Value"), 
          fr = paste0("Valeur")) |>
  add_row(en = paste0("Plot"), 
          fr = paste0("Représentation")) |>
  add_row(en = paste0("Bottom {per}"), 
          fr = paste0("{per} plus bas")) |>
  add_row(en = paste0("Outlier"), 
          fr = paste0("Donnée aberrante")) |>
  add_row(en = paste0("Extreme outlier"), 
          fr = paste0("Donnée aberrante extrême")) |>
  add_row(en = paste0("`Extreme outlier`: the variables rank in the ",
                      "top/bottom 10% of the {text_ior}."), 
          fr = paste0("`Donnée aberrante extrême` : les variables se classent ",
                      "dans les 10% supérieurs/inférieurs des {text_ior}.")) |>
  add_row(en = paste0("Typical"), 
          fr = paste0("Typique")) |>
  add_row(en = paste0("`Outlier`: the variables rank in the top/bottom 20% ",
                      "of the {text_ior}."), 
          fr = paste0("`Donnée aberrante` : les variables se classent dans ",
                      "les 20% supérieurs/inférieurs des {text_ior}.")) |>
  add_row(en = paste0("`Typical`: the variables rank in the middle 60% ",
                      "of the {text_ior}."), 
          fr = paste0("`Typique` : les variables se situent dans les ",
                      "60% moyens des {text_ior}.")) |>
  add_row(en = paste0("{geo_area} ranks in the top "), 
          fr = paste0("{geo_area} se classe dans le plus haut ")) |>
  add_row(en = paste0("{geo_area} ranks in the bottom "), 
          fr = paste0("{geo_area} se classe dans le plus bas ")) |>
  add_row(en = paste0("There were {z$pretty_data_var} total crashes per 1,000 ",
                      "residents in {z$data_date}. {z$data_rank}."), 
          fr = paste0("Il y a eu {z$pretty_data_var} collisions pour 1 000 ",
                      "habitants en {z$data_date}. {z$data_rank}")) |> 
  add_row(en = paste0("Its value is higher than the WHO's guideline value of 5. "), 
          fr = paste0("Sa valeur est supérieure à la valeur maximale recommandée par ",
                      "l'OMS, qui est de 5. ")) |>
  add_row(en = paste0("{z$data_rank} in terms of level of NO2 pollution. ",
                      "{higher_than_threshold}(NO2 = {z$pretty_data_var}, ",
                      "data from {z$data_date})"), 
          fr = paste0("{z$data_rank} en termes de niveau de pollution par le ",
                      "NO2. {higher_than_threshold}(NO2 = {z$pretty_data_var}, ",
                      "données de {z$data_date})")) |>
  add_row(en = paste0("{z$pretty_data_var} of occupied dwellings are ",
                      "single-detached houses. {z$data_rank}. (Data from {z$data_date})"), 
          fr = paste0("{z$pretty_data_var} des logements occupés sont des ",
                      "maisons individuelles. {z$data_rank}. (Données de {z$data_date})")) |>
  add_row(en = paste0("{z$data_rank} in terms of active living. (Data from {z$data_date})"), 
          fr = paste0("{z$data_rank} en termes de vie active. (Données de {z$data_date})")) |>
  add_row(en = paste0("No data."), 
          fr = paste0("Aucune donnée.")) |>
  add_row(en = paste0("{z$data_rank} in terms of green space. (<a ",
                      "href='https://www.canuedata.ca/tmp/CANUE_METADATA_GRAV",
                      "H_AMN_YY.pdf' target='_blank'>NDVI</a> = {z$pretty_data",
                      "_var}, data from {z$data_date})"), 
          fr = paste0("{z$data_rank} en termes d'espace vert. (<a href='https:",
                      "//www.canuedata.ca/tmp/CANUE_METADATA_GRAVH_AMN_YY.pd",
                      "f' target='_blank'>NDVI</a> = {z$pretty_data_var}, ",
                      "données de {z$data_date})")) |> 
  add_row(en = paste0("Sustainable transport"), 
          fr = paste0("Transport durable")) |> 
  add_row(en = paste0("Road collisions"), 
          fr = paste0("Collisions de la route")) |> 
  add_row(en = paste0("Air pollution"), 
          fr = paste0("Pollution de l'air")) |> 
  add_row(en = paste0("Housing"), 
          fr = paste0("Logement")) |> 
  add_row(en = paste0("Green space"), 
          fr = paste0("Espace vert")) |> 
  add_row(en = paste0("Active living"), 
          fr = paste0("Vie active")) |> 
  add_row(en = paste0("The area around "), 
          fr = paste0("La zone autour du ")) |> 
  add_row(en = paste0("It ranks {text_data_rank} {text_island_region}"), 
          fr = paste0("Il se classe {text_data_rank} {text_island_region}")) |> 
  add_row(en = paste0("relatively low at {ordinal_form(r = r, data_CSD_rank)}"), 
          fr = paste0("relativement bas au {ordinal_form(r = r, data_CSD_rank)} rang")) |> 
  add_row(en = paste0(" on the island"), 
          fr = paste0(" sur l'île")) |> 
  add_row(en = paste0(" in the region"), 
          fr = paste0(" dans la région")) |> 
  add_row(en = paste0("on the island"), 
          fr = paste0("sur l'île")) |> 
  add_row(en = paste0("in the region"), 
          fr = paste0("dans la région")) |> 
  add_row(en = paste0("the island"), 
          fr = paste0("l'île")) |> 
  add_row(en = paste0("the region"), 
          fr = paste0("la région")) |> 
  add_row(en = paste0("{ordinal_form(r = r, data_CSD_rank)} best"), 
          fr = paste0("en {ordinal_form(r = r, data_CSD_rank)} position")) |> 
  add_row(en = paste0("Its value is higher than {scale_percent_data_rank} of ",
                      "{geo_areas} {text_island_region}"), 
          fr = paste0("Sa valeur est supérieure à {scale_percent_data_rank} des ",
                      "{geo_areas} {text_island_region}")) |> 
  add_row(en = paste0("Its value is worse than {scale_percent_data_rank} of ",
                      "{geo_areas} {text_island_region}"), 
          fr = paste0("Sa valeur est pire que celle de {scale_percent_data_rank} des ",
                      "{geo_areas} {text_island_region}")) |> 
  add_row(en = paste0("the total number of jobs accessible within 30 minutes on average"), 
          fr = paste0("le nombre total d'emplois accessibles en 30 minutes et moins en moyenne")) |> 
  add_row(en = paste0("the number of low-skill jobs accessible within 30 minutes on average"), 
          fr = paste0("le nombre total d'emplois peu qualifiés accessibles en 30 minutes et moins en moyenne")) |> 
  add_row(en = paste0("the number of high-skill jobs accessible within 30 minutes on average"), 
          fr = paste0("le nombre total d'emplois hautement qualifiés accessibles en 30 minutes et moins en moyenne")) |> 
  add_row(en = paste0("the number of jobs paying less than $30,000 accessible within 30 minutes on average"), 
          fr = paste0("le nombre total d'emplois ayant un salaire de moins de 30 000$ accessibles en 30 minutes et moins en moyenne")) |> 
  add_row(en = paste0("the number of schools accessible within 30 minutes on average"), 
          fr = paste0("le nombre total d'écoles accessibles en 30 minutes et moins en moyenne")) |> 
  add_row(en = paste0("the number of healthcare facilities within 30 minutes on average"), 
          fr = paste0("le nombre total d'établissements de santé accessibles en 30 minutes et moins en moyenne")) |> 
  add_row(en = paste0("the number of healthcare facilities accessible within 30 minutes on average"), 
          fr = paste0("le nombre total d'établissements de santé accessibles en 30 minutes et moins en moyenne")) |> 
  add_row(en = paste0("City"), 
          fr = paste0("Ville")) |> 
  
  add_row(en = paste0("The area's residents are disproportionately in the {age} ",
                      "age range, compared to the rest of the {ior}."), 
          fr = paste0("Les résidents de la zone sont disproportionnellement dans ",
                      "la tranche d'âge {age}, par rapport au reste des {ior}.")) |> 
  add_row(en = paste0("much higher"), 
          fr = paste0("beaucoup plus élevé")) |> 
  add_row(en = paste0("slightly higher"), 
          fr = paste0("légèrement supérieur")) |> 
  add_row(en = paste0("slightly lower"), 
          fr = paste0("légèrement inférieur")) |> 
  add_row(en = paste0("lower"), 
          fr = paste0("inférieur")) |> 
  add_row(en = paste0("much lower"), 
          fr = paste0("bien inférieur")) |> 
  add_row(en = paste0("The area has a {more_less} level of climate risk than ",
                      "average for the {ior}."), 
          fr = paste0("La zone présente un niveau de risque climatique ",
                      "{more_less} à/que la moyenne des {ior}.")) |> 
  
  add_row(en = paste0("much more"), 
          fr = paste0("beaucoup plus")) |> 
  add_row(en = paste0("more"), 
          fr = paste0("plus")) |> 
  add_row(en = paste0("slightly more"), 
          fr = paste0("un peu plus")) |> 
  add_row(en = paste0("slightly less"), 
          fr = paste0("un peu moins")) |> 
  add_row(en = paste0("less"), 
          fr = paste0("moins")) |> 
  add_row(en = paste0("much less"), 
          fr = paste0("beaucoup moins")) |> 
  add_row(en = paste0("Residents of the area are {more_less} likely than the rest ",
                      "of the {ior} to have a university degree."), 
          fr = paste0("Les résidents de la zone sont {more_less} ",
                      "susceptibles que le reste des {ior} d'avoir un diplôme ",
                      "universitaire.")) |> 
  
  add_row(en = paste0("A {more_less} than average share of the area's residents ",
                      "work in creative and professional occupations compared to ",
                      "the rest of the {ior}."), 
          fr = paste0("Une part {more_less} à/que la moyenne des résidents ",
                      "de la zone travaille dans des professions créatives et ",
                      "professionnelles, par rapport au reste des {ior}.")) |> 
  
  add_row(en = paste0("much larger"), 
          fr = paste0("beaucoup plus grand/s/e/es")) |> 
  add_row(en = paste0("larger"), 
          fr = paste0("plus grand/s/e/es")) |> 
  add_row(en = paste0("slightly larger"), 
          fr = paste0("un peu plus grand/s/e/es")) |> 
  add_row(en = paste0("slightly smaller"), 
          fr = paste0("un peu plus petit/s/e/es")) |> 
  add_row(en = paste0("smaller"), 
          fr = paste0("plus petit/s/e/es")) |> 
  add_row(en = paste0("much smaller"), 
          fr = paste0("beaucoup plus petit/s/e/es")) |> 
  add_row(en = paste0("The area's families are {more_less} than average for the {ior}."), 
          fr = paste0("Les familles de la région sont {more_less} que la moyenne des {ior}.")) |> 
  
  add_row(en = paste0("much more expensive"), 
          fr = paste0("beaucoup plus élevé")) |> 
  add_row(en = paste0("more expensive"), 
          fr = paste0("plus élevé")) |> 
  add_row(en = paste0("slightly more expensive"), 
          fr = paste0("un peu plus élevé")) |> 
  add_row(en = paste0("slightly cheaper"), 
          fr = paste0("un peu moins élevé")) |> 
  add_row(en = paste0("cheaper"), 
          fr = paste0("moins élevés")) |> 
  add_row(en = paste0("much cheaper"), 
          fr = paste0("beaucoup moins élevé")) |> 
  add_row(en = paste0("Housing costs in the area are {more_less} than average ",
                      "for the {ior}."), 
          fr = paste0("Le coût du logement dans la zone est {more_less} que ",
                      "la moyenne des {ior}.")) |> 
  
  add_row(en = paste0("slightly fewer"), 
          fr = paste0("un peu moins")) |> 
  add_row(en = paste0("fewer"), 
          fr = paste0("moins")) |> 
  add_row(en = paste0("much fewer"), 
          fr = paste0("beaucoup moins")) |> 
  add_row(en = paste0("The area has {more_less} foreign-born residents than ",
                      "average for the {ior}."), 
          fr = paste0("La zone compte {more_less} de résidents nés à ",
                      "l'étranger que la moyenne des {ior}.")) |> 
  
  add_row(en = paste0("Incomes in the area are {more_less} than average for the {ior}."), 
          fr = paste0("Les revenus dans la zone sont {more_less} que la moyenne des {ior}.")) |> 
  
  add_row(en = paste0("speak English"), 
          fr = paste0("parler anglais")) |> 
  add_row(en = paste0("speak French"), 
          fr = paste0("parler français")) |> 
  add_row(en = paste0("be bilingual (French and English)"), 
          fr = paste0("parler bilingue (français et anglais)")) |> 
  add_row(en = paste0("speak neither French nor English"), 
          fr = paste0("ne parler ni français ni anglais")) |> 
  add_row(en = paste0("The area's residents are {more_less} likely to {lang} ",
                      "than average for the {ior}."), 
          fr = paste0("Les habitants de la zone sont {more_less} susceptibles de ",
                      "{lang} que la moyenne de {ior}.")) |> 
  
  add_row(en = paste0("Residents in the area drive to work {more_less} than ",
                      "average compared to the rest of the {ior}."), 
          fr = paste0("Les habitants de la région se rendent {more_less} au ",
                      "travail en voiture que la moyenne par rapport au ",
                      "reste des {ior}.")) |> 
  
  add_row(en = paste0("The area has {more_less} public transit access to jobs ",
                      "than the rest of the {ior}."), 
          fr = paste0("La zone dispose d'un accès aux emplois par les ",
                      "transports en commun {more_less} important que le reste des {ior}."))
  