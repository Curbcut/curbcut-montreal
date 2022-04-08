### Place explorer translation #################################################

place_explorer_translated <- 
  tibble(en = character(), fr = character()) |>
  add_row(en = paste0("Island"), 
          fr = paste0("L'île")) |>
  add_row(en = paste0("Region"), 
          fr = paste0("Grande région")) |>
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
                      "top/bottom 10% relative to {text_island_region}."), 
          fr = paste0("`Donnée aberrante extrême`: les variables se classent ",
                      "dans les 10% supéreurs/inférieurs par rapport à ",
                      "{text_island_region}")) |>
  add_row(en = paste0("Typical"), 
          fr = paste0("Typique")) |>
  add_row(en = paste0("`Outlier`: the variables rank in the top/bottom 20% ",
                      "relative to {text_island_region}."), 
          fr = paste0("`Donnée aberrante` : les variables se classent dans ",
                      "les 20% supérieurs/ inférieurs par rapport à ",
                      "{text_island_region}")) |>
  add_row(en = paste0("`Typical`: the variables rank in the middle 60% ",
                      "relative to {text_island_region}."), 
          fr = paste0("`Typique` : les variables se situent dans la ",
                      "fourchette intermédiaire (entre les 20% et 80%) ",
                      "par rapport à {text_island_region}.")) |>
  add_row(en = paste0("{geo_area} ranks in the {text_high_is_good} "), 
          fr = paste0("{geo_area} se classe dans le {text_high_is_good} ")) |>
  add_row(en = paste0("There were {z$pretty_data_var} total crashes per 1,000 ",
                      "residents in {z$data_date}. {z$data_rank}."), 
          fr = paste0("Il y a eu {z$pretty_data_var} collisions pour 1 000 ",
                      "habitants en {z$data_date}. {z$data_rank}")) |> 
  add_row(en = paste0("Its value is higher than WHO's guideline value of 5. "), 
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
  add_row(en = paste0("relatively low at {ordinal_form(data_rank)}"), 
          fr = paste0("relativement bas au {ordinal_form(data_rank)} rang")) |> 
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
  add_row(en = paste0("{ordinal_form(data_rank)} best"), 
          fr = paste0("en {ordinal_form(data_rank)} position")) |> 
  add_row(en = paste0("Its value is higher than {scale_percent_data_rank} of ",
                      "{geo_areas} {text_island_region}"), 
          fr = paste0("Sa valeur est supérieure à {scale_percent_data_rank} des ",
                      "{geo_areas} {text_island_region}.")) |> 
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
  add_row(en = paste0("Residents of the area have a {more_less} proportion of university degrees than average for {island_or_region}."), 
          fr = paste0("Les résidents de la zone ont une {more_less} proportion de diplômes universitaires en rapport à la moyenne de {island_or_region}.")) |> 
  add_row(en = paste0("less"), 
          fr = paste0("moins")) |> 
  add_row(en = paste0("more"), 
          fr = paste0("plus")) |> 
  add_row(en = paste0("Residents in the area drive to work {more_less} than average for {island_or_region}."), 
          fr = paste0("Les habitants de la région se rendent {more_less} au travail en voiture que la moyenne de {island_or_region}.")) |> 
  add_row(en = paste0("Property values in the area are higher than average for {island_or_region}."), 
          fr = paste0("La valeur des propriétés dans la zone est supérieure à la moyenne de {island_or_region}.")) |> 
  add_row(en = paste0("are more bilingual (French and English)"), 
          fr = paste0("sont plus bilingues (français et anglais)")) |> 
  add_row(en = paste0("The area's residents {lang} than average for {island_or_region}."), 
          fr = paste0("Les habitants de la zone {lang} que la moyenne de {island_or_region}.")) |> 
  add_row(en = paste0("speak more English"), 
          fr = paste0("parlent plus l'anglais")) |> 
  add_row(en = paste0("speak more French"), 
          fr = paste0("parlent plus le français")) |> 
  add_row(en = paste0("Incomes in the area are {higher_lower} than average for {island_or_region}."), 
          fr = paste0("Les revenus de la région sont {higher_lower} que la moyenne de {island_or_region}.")) |> 
  add_row(en = paste0("The area has {more_less} foreign-born residents than average for {island_or_region}."), 
          fr = paste0("La zone compte {more_less} de résidents nés à l'étranger que la moyenne de {island_or_region}.")) |> 
  add_row(en = paste0("The area's population is {older_younger} than average for {island_or_region}."), 
          fr = paste0("La population de la zone est {older_younger} que la moyenne de {island_or_region}.")) |> 
  add_row(en = paste0("older"), 
          fr = paste0("plus vieille")) |> 
  add_row(en = paste0("younger"), 
          fr = paste0("plus jeune")) |> 
  add_row(en = paste0("The area's percentage of tenants is higher than average for {island_or_region}."), 
          fr = paste0("Le pourcentage de locataires de la zone est supérieur à la moyenne de {island_or_region}."))