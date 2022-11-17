
## Disclaimer and dyk
# form_translation_tibble(read.csv("dev/data/city_accessibility/translation.csv",
#                                  encoding = "latin1") |>
#   mutate(en = str_trim(en),
#          fr = str_trim(fr)))
# 
# # Creating df used to add translations to the script ----------------------
# 
# # Variables needed translation
# variables_city <- variables[str_starts(variables$var_code, "city_amenities"), ]
# 
# variables_city_translated <-
#   map_dfr(variables_city$var_code, function(var) {
# 
#     # TITLE
#     category <- case_when(str_detect(var, "_library_") ~
#                             "de bibliothèques",
#                           str_detect(var, "_cultural_") ~
#                             "d'activités culturelles",
#                           str_detect(var, "_communita_") ~
#                             "d'équipements communautaires",
#                           str_detect(var, "_daycare_") ~
#                             "de garderies et CPE",
#                           str_detect(var, "_preschool_") ~
#                             "de services de maternelle 4 ans",
#                           str_detect(var, "_primary_") ~
#                             "d'établissements scolaires primaires",
#                           str_detect(var, "_secondary_") ~
#                             "d'établissements scolaires secondaires",
#                           str_detect(var, "_postsecondary_") ~
#                             "d'établissements postsecondaires",
#                           str_detect(var, "_sport_inte_") ~
#                             "d'activités sportives intérieures",
#                           str_detect(var, "_equi_sante_") ~
#                             "d'équipements de santé et de services sociaux",
#                           str_detect(var, "_commercial_zone_") ~
#                             "de zones commerciales",
#                           str_detect(var, "_alimentation_") ~
#                             "de commerces alimentaires",
#                           str_detect(var, "_pharmacy_") ~
#                             "de pharmacies",
#                           str_detect(var, "_laundromat_") ~
#                             "de buanderies",
#                           str_detect(var, "_p_quartier_") ~
#                             "de parcs de quartier",
#                           str_detect(var, "_big_park_") ~
#                             "de grands parcs",
#                           str_detect(var, "_playground_") ~
#                             "d'aires de jeux",
#                           str_detect(var, "_cemeteries_") ~
#                             "de cimetières",
#                           str_detect(var, "_nature_") ~
#                             "d'activités nature",
#                           str_detect(var, "_aquatic_") ~
#                             "d'activités extérieures aquatiques",
#                           str_detect(var, "_winter_") ~
#                             "d'activités extérieures d'hiver",
#                           str_detect(var, "_summer_") ~
#                             "d'activités extérieures d'été")
# 
#     mode_title <- case_when(str_detect(var, "_walk_") ~
#                               "(Marche)",
#                             str_detect(var, "_bike_") ~
#                               "(Vélo)")
# 
#     mode_exp <- case_when(str_detect(var, "_walk_") ~
#                             "15 minutes à la marche",
#                           str_detect(var, "_bike_") ~
#                             "20 minutes à vélo")
# 
#     title <- paste0("Accessibilité aux ", str_remove(category, "(de )|(d')"), " ",
#                     mode_title)
#     short <- str_to_sentence(str_remove(category, "(de )|(d')"))
#     exp <- paste0("le nombre moyen ", category,
#                   " accessibles par un résident moyen de la zone en moins de ",
#                   mode_exp)
# 
#     # ADDED ROW
# 
#     tibble(var = var,
#            group = c("title", "short", "explanation", "list_var"),
#            en = c(
#              # Title
#              variables_city$var_title[variables_city$var_code == var],
#              # Short
#              variables_city$var_short[variables_city$var_code == var],
#              # Exp
#              variables_city$explanation[variables_city$var_code == var],
#              # List var
#              variables_city$var_title[variables_city$var_code == var] |>
#                str_remove(" \\(.*")),
#            fr = c(title, short, exp, str_remove(title, " \\(.*")))
#   })

city_amenities_translation <- 
  tibble(en = character(), fr = character()) |>
  
  add_row(en = paste0("City amenities"), 
          fr = paste0("Commodité")) |> 
  
  add_row(en = paste0("Outremont is the borough with the highest accessibility",
                      " to daycares with an average of 8 daycares within a 15-",
                      "minute walk."), 
          fr = paste0("Outremont est l'arrondissement où l'accessibilité aux g",
                      "arderies à la marche est la plus élevée avec une moyenn",
                      "e de 8 garderies à moins de 15 minutes de marche.")) |> 
  add_row(en = paste0("More than 80% of Plateau-Mont-Royal residents have acce",
                      "ss to at least three primary schools within a 15-minute",
                      " walk, making it the borough with the best accessibilit",
                      "y in Montreal. It is also the borough with the lowest p",
                      "roportion of people who do not have access to any eleme",
                      "ntary school within a 15-minute walk."), 
          fr = paste0("Plus de 80% des habitants du Plateau-Mont-Royal ont acc",
                      "ès à au moins trois écoles primaires à moins de 15 minu",
                      "tes à la marche, ce qui en fait l'arrondissement de Mon",
                      "tréal avec la meilleure accessibilité. C'est également ",
                      "l'arrondissement pour lequel la proportion de personnes",
                      " n'ayant accès à aucune école primaire à moins de 15 mi",
                      "nutes de marche est la plus faible.")) |> 
  add_row(en = paste0("Roughly 55% of households with income lower than $50,00",
                      "0 do not have access to a secondary school within a 15-",
                      "minute walk and only 3% have access to more than five s",
                      "econdary schools within that threshold."), 
          fr = paste0("Environ 55 % des ménages dont le revenu est inférieur à",
                      " 50 000$ n'ont accès à aucune école secondaire à moins ",
                      "de 15 minutes de marche et seulement 3 % ont accès à pl",
                      "us de cinq écoles secondaires dans ce même seuil.")) |> 
  add_row(en = paste0("Ville-Marie  is the borough with the highest accessibil",
                      "ity to postsecondary schools with an average of 5 schoo",
                      "ls within a 15-minute walk."), 
          fr = paste0("Ville-Marie est l'arrondissement où l'accessibilité aux",
                      " écoles postsecondaires est la plus élevée, avec une mo",
                      "yenne de 5 écoles à moins de 15 minutes de marche.")) |> 
  add_row(en = paste0("Villeray-Saint-Michel-Parc-Extension is the borough wit",
                      "h the highest accessibility to indoor sports activities",
                      " with an average of 1.6 activities within a 15-minute w",
                      "alk."), 
          fr = paste0("Villeray-Saint-Michel-Parc-Extension est l'arrondisseme",
                      "nt où l'accessibilité aux activités sportives intérieur",
                      "es est la plus élevée, avec une moyenne de 1,6 activité",
                      " à moins de 15 minutes de marche.")) |> 
  add_row(en = paste0("Health and social services facilities are mainly locate",
                      "d in Côte-des-Neiges-Notre-Dame-de-Grâce, Ville-Marie a",
                      "nd Rosemont-La-Petite-Patrie; thus, these boroughs  off",
                      "er the best accessibility by walk to these destinations",
                      "."), 
          fr = paste0("Les équipements de santé et de services sociaux sont ne",
                      "ttement concentrés dans les arrondissements de Côte-des",
                      "-Neiges–Notre-Dame-de-Grâce, Ville-Marie et Rosemont-La",
                      "-Petite-Patrie ; ce sont donc ces arrondissements qui o",
                      "ffrent la meilleure accessibilité à ces destinations, à",
                      " pied et à vélo.")) |> 
  add_row(en = paste0("Rosemont-La-Petite-Patrie offers the best accessibility",
                      " by walk to food stores, followed by Ville-Marie and Pl",
                      "ateau-Mont-Royal. On the contrary, the worst accessibil",
                      "ity is observed in Île-Bizard-Sainte-Geneviève, where m",
                      "ore than half of the residents do not have access to an",
                      "y food stores within a 15-minute walk."), 
          fr = paste0("L'arrondissement Rosemont-La-Petite-Patrie offre la mei",
                      "lleure accessibilité aux commerces alimentaires à la ma",
                      "rche, suivi de Ville-Marie et du Plateau-Mont-Royal. Au",
                      " contraire, le pire niveau d'accessibilité s'observe su",
                      "r l'Île-Bizard-Sainte-Geneviève, où plus de la moitié d",
                      "es habitants n'a accès à aucun commerce alimentaire en ",
                      "15 minutes à pied.")) |> 
  add_row(en = paste0("Around 75,000 (19%) low-income households do not have a",
                      "ccess to a pharmacy within a 15-minute walk, and only 1",
                      "0% have access to more than five pharmacies."), 
          fr = paste0("Environ 75 000 (19 %) des ménages à faibles revenus n'o",
                      "nt accès à aucune pharmacie dans un rayon de 15 minutes",
                      " de marche et seulement 10 % ont accès à plus de cinq p",
                      "harmacies.")) |> 
  add_row(en = paste0("Ville-Marie, Plateau-Mont-Royal and Rosemont-La-Petite-",
                      "Patrie is where the highest accessibility to neighbourh",
                      "ood parks by walk is observed."), 
          fr = paste0("Les arrondissements de Ville-Marie, du Plateau-Mont-Roy",
                      "al et de Rosemont-La-Petite-Patrie sont ceux qui possèd",
                      "ent la meilleure accesibilité aux parcs de quartiers, e",
                      "t ce, à pied et à vélo.")) |> 
  add_row(en = paste0("Around one fifth of the households with income lower th",
                      "an $50,000 have access to more than a big park within a",
                      " 15-minute walk, and only 0.4% have access to more than",
                      " five big parks."), 
          fr = paste0("Environ un cinquième des ménages dont le revenu est inf",
                      "érieur à 50 000$ ont accès à plus d'un grand parc dans ",
                      "un rayon de 15 minutes de marche, et seulement 0,4 % on",
                      "t accès à plus de cinq grands parcs.")) |> 
  add_row(en = paste0("Montréal-Nord is the borough with the highest accessibi",
                      "lity to outdoor water activities  with an average of 1.",
                      "7 activities within a 15-minute walk."), 
          fr = paste0("Montréal-Nord est l'arrondissement où l'accessibilité a",
                      "ux activités extérieures aquatiques est la plus élevée,",
                      " avec une moyenne de 1,7 activité à moins de 15 minutes",
                      " de marche.")) |> 
  add_row(en = paste0("Saint-Léonard is the borough with the highest accessibi",
                      "lity to outdoor summer activities with an average of 8 ",
                      "activities within a 15-minute walk."), 
          fr = paste0("Saint-Léonard est l'arrondissement où l'accessibilité a",
                      "ux activités extérieures d'été est la plus élevée, avec",
                      " une moyenne de 8 activités à 15 minutes de marche.")) |> 
  add_row(en = paste0("Outremont is the borough with the highest accessibility",
                      " to daycares with an average of 70 daycares within a 20",
                      "-minute bike ride."), 
          fr = paste0("Outremont est l'arrondissement où l'accessibilité aux g",
                      "arderies à vélo est la plus élevée avec une moyenne de ",
                      "70 garderies à moins de 20 minutes de marche.")) |> 
  add_row(en = paste0("In every Montreal boroughs except Île-Bizard-Sainte-Gen",
                      "eviève, Pierrefonds-Roxboro and Anjou, 95% of citizens ",
                      "have access to at least three primary schools within a ",
                      "20-minute bike ride."), 
          fr = paste0("Dans tous les arrondissements de Montréal, à l'exceptio",
                      "n de l'Île-Bizard-Sainte-Geneviève, Pierrefonds-Roxboro",
                      " et Anjou, 95% des citoyens ont accès à au moins trois ",
                      "écoles primaines à moins de 20 minutes en vélo.")) |> 
  add_row(en = paste0("47,710 (5%) households in Montreal do not have access t",
                      "o a secondary school within a 20-minute bike ride but o",
                      "nly 3.5% of the households with income lower than $50,0",
                      "00 do not have access to a secondary school and 54% hav",
                      "e access to less than the city average (9.67)."), 
          fr = paste0("47 710 (5 %) des ménages de Montréal n'ont pas accès à ",
                      "une école secondaire à moins de 20 minutes de vélo, mai",
                      "s seulement 3,5 % des ménages dont le revenu est inféri",
                      "eur à 50 000 $ n'ont pas accès à une école secondaire e",
                      "t 54 % ont accès à moins que la moyenne de la ville (9,",
                      "67).")) |> 
  add_row(en = paste0("Le Plateau-Mont-Royal is the borough with the highest a",
                      "ccessibility to postsecondary schools with an average o",
                      "f 47 schools within a 20-minute bike ride."), 
          fr = paste0("Le Plateau-Mont-Royal est l'arrondissement ayant la plu",
                      "s grande accessibilité aux écoles postsecondaires avec ",
                      "une moyenne de 47 écoles à moins de 20 minutes de vélo.",
                      "")) |> 
  add_row(en = paste0("Le Plateau-Mont-Royal, Rosemont-La-Petite-Patrie, Ville",
                      "ry-Saint-Michel-Parc-Extension, Sud-Ouest and Côte-des-",
                      "Neiges-Notre-Dame-de-Grâce offer a lot of indoor sports",
                      " activities and are the boroughs where accessibility to",
                      " these activities by bike is the highest (between 12 an",
                      "d 30 destinations per diffusion block)."), 
          fr = paste0("Le Plateau-Mont-Royal, Rosemont-La-Petite-Patrie, Ville",
                      "ry-Saint-Michel-Parc-Extension, le Sud-Ouest et Côte-de",
                      "s-Neiges-Notre-Dame-de-Grâce sont des arrondissements q",
                      "ui offrent beaucoup d'activités sportives intérieures e",
                      "t où l'accessibilité à celles-ci en vélo est la plus él",
                      "evée (entre 12 et 30 destinations accessibles par îlot ",
                      "de diffusion).")) |> 
  add_row(en = paste0("Health and social services facilities are mainly locate",
                      "d in Côte-des-Neiges-Notre-Dame-de-Grâce, Ville-Marie a",
                      "nd Rosemont-La-Petite-Patrie; thus, these boroughs  off",
                      "er the best accessibility by bike to these destinations",
                      "."), 
          fr = paste0("Les équipements de santé et de services sociaux sont ne",
                      "ttement concentrés dans les arrondissements de Côte-des",
                      "-Neiges–Notre-Dame-de-Grâce, Ville-Marie et Rosemont-La",
                      "-Petite-Patrie ; ce sont donc ces arrondissements qui o",
                      "ffrent la meilleure accessibilité à ces destinations, à",
                      " pied et à vélo.")) |> 
  add_row(en = paste0("Rosemont-La-Petite-Patrie offers the best accessibility",
                      " by bike to food stores, followed by Ville-Marie and Pl",
                      "ateau-Mont-Royal. On the contrary, the worst accessibil",
                      "ity is observed in Île-Bizard-Sainte-Geneviève, where j",
                      "ust over 10% of the residents do not have access to any",
                      " food stores within a 20-minute bike ride."), 
          fr = paste0("L'arrondissement Rosemont-La-Petite-Patrie offre la mei",
                      "lleure accessibilité aux commerces alimentaires à vélo,",
                      " suivi de Ville-Marie et du Plateau-Mont-Royal. Au cont",
                      "raire, le pire niveau d'accessibilité s'observe sur l'Î",
                      "le-Bizard-Sainte-Geneviève, où un peu plus de 10% des h",
                      "abitants n'a accès à aucun commerce alimentaire en 20 m",
                      "inutes à vélo.")) |> 
  add_row(en = paste0("Around 200,000 (53%) households with income lower than ",
                      "$50,000 have access to less than 18 pharmacies within a",
                      " 20-minute bike ride,  which is the average accessibili",
                      "ty in Montreal."), 
          fr = paste0("Environ 200 000 (53 %) ménages dont le revenu est infér",
                      "ieur à 50 000$ ont accès à moins de 18 pharmacies dans ",
                      "un rayon de 20 minutes à vélo, ce qui correspond à l'ac",
                      "cessibilité moyenne à Montréal.")) |> 
  add_row(en = paste0("Ville-Marie, Plateau-Mont-Royal and Rosemont-La-Petite-",
                      "Patrie is where the highest accessibility to neighbourh",
                      "ood parks by bike is observed. In all Montreal borough",
                      "s, more than 9 out of 10 people have access to more tha",
                      "n three neighbourhood parks within a 20-minute bike rid",
                      "e."), 
          fr = paste0("Les arrondissements de Ville-Marie, du Plateau-Mont-Roy",
                      "al et de Rosemont-La-Petite-Patrie sont ceux qui possèd",
                      "ent la meilleure accesibilité aux parcs de quartiers, e",
                      "t ce, à pied et à vélo. Dans tous les arrondissements,",
                      " plus de 9 personnes sur 10 ont accès à plus de trois p",
                      "arcs de quartiers à moins de 20 minutes à vélo.")) |> 
  add_row(en = paste0("Around 115,000 (30%) households with income lower than ",
                      "$50,000 do not have access to any big park within a 20-",
                      "minute bike ride. Only 13% have access to more than fiv",
                      "e big parks."), 
          fr = paste0("Environ 115 000 (30%) des ménages dont le revenu est in",
                      "férieur à 50 000$ n'ont accès à aucun grand parc dans u",
                      "n rayon de 20 minutes de vélo. Seuls 13% ont accès à pl",
                      "us de cinq grands parcs.")) |> 
  add_row(en = paste0("Montréal-Nord is the borough with the highest accessibi",
                      "lity to outdoor water activities with an average of 10 ",
                      "activities within a 20-minute bike ride."), 
          fr = paste0("Montréal-Nord est l'arrondissement où l'accessibilité a",
                      "ux activités extérieures aquatiques est la plus élevée,",
                      " avec une moyenne de 10 activités à moins de 20 minutes",
                      " de vélo.")) |> 
  add_row(en = paste0("Rosemont-La Petite-Patrie  is the borough with the high",
                      "est accessibility to outdoor summer activities with an ",
                      "average of 85 activities within a 20-minute walk."), 
          fr = paste0("Rosemont-La Petite-Patrie est l'arrondissement où l'acc",
                      "essibilité aux activités extérieures d'été est la plus ",
                      "élevée, avec une moyenne de 85 activités à moins de 20 ",
                      "minutes de marche.")) |> 
  add_row(en = paste0("Cultural facilities include centres of culture, arts ce",
                      "ntres, outdoor theater scenes, and public and private m",
                      "useums."), 
          fr = paste0("Les activités culturelles incluent les centres culturel",
                      "s, les maisons de la culture, les scènes de théâtre ext",
                      "érieurs et les musées publics et privés.")) |> 
  add_row(en = paste0("Community facilities include community and recreation c",
                      "enters, and community and humanitarian association faci",
                      "lities."), 
          fr = paste0("Les équipements communautaires incluent les centres com",
                      "munautaires et de loisirs et les locaux d'associations ",
                      "communautaires et humanitaires.")) |> 
  add_row(en = paste0("This category includes subsidized and non-subsidized pr",
                      "ivate daycares and childcare centers."), 
          fr = paste0("Cette catégorie inclut les garderies privées subvention",
                      "nées ou non-subventionnées et les centres de la petite ",
                      "enfance (CPE).")) |> 
  add_row(en = paste0("This category includes public and private primary schoo",
                      "ls."), 
          fr = paste0("Cette catégorie inclut les écoles primaires publiques e",
                      "t privées.")) |> 
  add_row(en = paste0("This category includes public and private secondary sch",
                      "ools."), 
          fr = paste0("Cette catégorie inclut les écoles secondaires publiques",
                      " et privées.")) |> 
  add_row(en = paste0("This category includes higher education institutions (C",
                      "EGEPs, universities) and vocational and adult education",
                      " centers."), 
          fr = paste0("Cette catégorie inclut les établissements d'enseignemen",
                      "t supérieur (cégeps, universités) et les centres de for",
                      "mation professionnelle et d'éducation aux adultes.")) |> 
  add_row(en = paste0("Indoor sports activities include indoor pools, arenas, ",
                      "and sports centers and complexes."), 
          fr = paste0("Les activités sportives intérieures incluent les piscin",
                      "es intérieures, les arénas et les centres et complexes ",
                      "sportifs.")) |> 
  add_row(en = paste0("Health and social services facilities include local com",
                      "munity services centre (CLSC), family medicine group (F",
                      "MG), and medical clinics."), 
          fr = paste0("Les équipements de santé et de services sociaux incluen",
                      "t les centres locaux de services communautaires (CLSC),",
                      " les groupes de médecine de famille (GMF) et les cliniq",
                      "ues médicales.")) |> 
  add_row(en = paste0("This category includes stores on commercial streets and",
                      " as well as in shopping malls."), 
          fr = paste0("Cette catégorie inclut les artères et zones commerciale",
                      "s ainsi que les centres commerciaux.")) |> 
  add_row(en = paste0("Food stores include grocery stores, specialized grocery",
                      " stores (dairy stores, butcher shops, etc), and public ",
                      "markets."), 
          fr = paste0("Les commerces alimentaires incluent les épiceries, les ",
                      "épiceries spécialisées et les marchés publics.")) |> 
  add_row(en = paste0("This category includes metropolitan, urban and linear p",
                      "arks; nature parks and arboreta, national parks, histor",
                      "ic sites, neighbourhood parks, public squares and river",
                      "front strips. The area of the parks is not considered ",
                      "in the calculations."), 
          fr = paste0("Cette catégorie inclut les parcs métropolitains,  urbai",
                      "ns et linéaires, les parcs nature et les arboretums, le",
                      "s parcs nationaux, les lieux historiques, les parcs de ",
                      "quartier et de voisinage, les places publiques, les ban",
                      "des rivenaires et les squares. La superficie des parcs ",
                      "n'est pas considérée dans les calculs.")) |> 
  add_row(en = paste0("This category includes metropolitan and urban parks, na",
                      "ture parks and arboreta, national parks, and historic s",
                      "ites. The area of the parks is not considered in the ca",
                      "lculations."), 
          fr = paste0("Cette catégorie inclut les parcs métropolitains et urba",
                      "ins, les parcs nature et les arboretums, les parcs nati",
                      "onaux et les lieux historiques. La superficie des parcs",
                      " n'est pas considérée dans les calculs.")) |> 
  add_row(en = paste0("This category includes playgrounds for children of all ",
                      "ages as well as schoolyards."), 
          fr = paste0("Cette catégorie inclut les aires de jeux destinés à des",
                      " enfants de tout âge ainsi que les parcs et les cours d",
                      "'école.")) |> 
  add_row(en = paste0("This category includes pedestrian streets, shared stree",
                      "ts, and pedestrianized commercial arteries."), 
          fr = paste0("Cette catégorie inclut les rues piétonnes, les rues par",
                      "tagées et les artères commerciales piétonnisées.")) |> 
  add_row(en = paste0("Nature activities include picnic areas, sun shelters, b",
                      "otanical gardens, and beaches."), 
          fr = paste0("Les activités nature incluent les aires de pique-nique,",
                      " les abris soleils, les jardins botaniques et les plage",
                      "s et grèves.")) |> 
  add_row(en = paste0("Outdoor water activities include outdoor pools, wading ",
                      "pools, and water games."), 
          fr = paste0("Les activités extérieures aquatiques incluent les pisci",
                      "nes extérieures et les pataugeoires et jeux d'eau.")) |> 
  add_row(en = paste0("Outdoor winter activities include sledding hills, skati",
                      "ng rinks, hiking, cross-country skiing and snowshoeing ",
                      "trails, and dog parks."), 
          fr = paste0("Les activités extérieures d'hiver incluent les aires de",
                      " glissade sur neige, les patinoires, les sentiers pédes",
                      "tres, de ski de fond et de raquette et les parcs canins",
                      ".")) |> 
  add_row(en = paste0("Outdoor summer activities include outdoor sports fields",
                      ", tracks and circuits, archery areas, hiking and multi-",
                      "purpose trails, training areas, marinas, boat launches ",
                      "and docks, climbing walls, licensed fishing areas, dog ",
                      "parks and 4-season skating rinks."), 
          fr = paste0("Les activités extérieures d'été incluent les terrains, ",
                      "pistes et circuits sportifs extérieurs, les aires de ti",
                      "r à l'arc, les sentiers pédestres et multifonctionnels,",
                      " les aires d’entrainement, les marinas, descentes de mi",
                      "se à l'eau et quais, les blocs et murs d’escalade, les ",
                      "aires de pêche autorisée, les parcs canins et les patin",
                      "oires 4 saisons.")) |> 
  
  
  add_row(en = paste0("Accessibility to cultural facilities (Cycling)"), 
          fr = paste0("Accessibilité aux activités culturelles (Vélo)")) |> 
  add_row(en = paste0("Cultural facilities"), 
          fr = paste0("Activités culturelles")) |> 
  add_row(en = paste0("the average number of cultural facilities a resident of",
                      " the area can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen d'activités culturelles accessibles par",
                      " un résident moyen de la zone en moins de 20 minutes à ",
                      "vélo")) |> 
  add_row(en = paste0("Accessibility to cultural facilities"), 
          fr = paste0("Accessibilité aux activités culturelles")) |> 
  add_row(en = paste0("Accessibility to daycares (Cycling)"), 
          fr = paste0("Accessibilité aux garderies et CPE (Vélo)")) |> 
  add_row(en = paste0("Daycares"), 
          fr = paste0("Garderies et cpe")) |> 
  add_row(en = paste0("the average number of daycares a resident of the area c",
                      "an reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de garderies et CPE accessibles par un ",
                      "résident moyen de la zone en moins de 20 minutes à vélo",
                      "")) |> 
  add_row(en = paste0("Accessibility to daycares"), 
          fr = paste0("Accessibilité aux garderies et CPE")) |> 
  add_row(en = paste0("Accessibility to preschools (Cycling)"), 
          fr = paste0("Accessibilité aux services de maternelle 4 ans (Vélo)")) |> 
  add_row(en = paste0("Preschools"), 
          fr = paste0("Services de maternelle 4 ans")) |> 
  add_row(en = paste0("the average number of preschools a resident of the area",
                      " can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de services de maternelle 4 ans accessi",
                      "bles par un résident moyen de la zone en moins de 20 mi",
                      "nutes à vélo")) |> 
  add_row(en = paste0("Accessibility to preschools"), 
          fr = paste0("Accessibilité aux services de maternelle 4 ans")) |> 
  add_row(en = paste0("Accessibility to primary scools (Cycling)"), 
          fr = paste0("Accessibilité aux établissements scolaires primaires (V",
                      "élo)")) |> 
  add_row(en = paste0("Primary scools"), 
          fr = paste0("Établissements scolaires primaires")) |> 
  add_row(en = paste0("the average number of primary scools a resident of the ",
                      "area can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen d'établissements scolaires primaires ac",
                      "cessibles par un résident moyen de la zone en moins de ",
                      "20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to primary scools"), 
          fr = paste0("Accessibilité aux établissements scolaires primaires")) |> 
  add_row(en = paste0("Accessibility to secondary schools (Cycling)"), 
          fr = paste0("Accessibilité aux établissements scolaires secondaires ",
                      "(Vélo)")) |> 
  add_row(en = paste0("Secondary schools"), 
          fr = paste0("Établissements scolaires secondaires")) |> 
  add_row(en = paste0("the average number of secondary schools a resident of t",
                      "he area can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen d'établissements scolaires secondaires ",
                      "accessibles par un résident moyen de la zone en moins d",
                      "e 20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to secondary schools"), 
          fr = paste0("Accessibilité aux établissements scolaires secondaires")) |> 
  add_row(en = paste0("Accessibility to postsecondary schools (Cycling)"), 
          fr = paste0("Accessibilité aux établissements postsecondaires (Vélo)",
                      "")) |> 
  add_row(en = paste0("Postsecondary schools"), 
          fr = paste0("Établissements postsecondaires")) |> 
  add_row(en = paste0("the average number of postsecondary schools a resident ",
                      "of the area can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen d'établissements postsecondaires access",
                      "ibles par un résident moyen de la zone en moins de 20 m",
                      "inutes à vélo")) |> 
  add_row(en = paste0("Accessibility to postsecondary schools"), 
          fr = paste0("Accessibilité aux établissements postsecondaires")) |> 
  add_row(en = paste0("Accessibility to indoor sports activities (Cycling)"), 
          fr = paste0("Accessibilité aux activités sportives intérieures (Vélo",
                      ")")) |> 
  add_row(en = paste0("Indoor sports activities"), 
          fr = paste0("Activités sportives intérieures")) |> 
  add_row(en = paste0("the average number of indoor sports activities a reside",
                      "nt of the area can reach within a 20-minute cycling ti",
                      "me"), 
          fr = paste0("le nombre moyen d'activités sportives intérieures acces",
                      "sibles par un résident moyen de la zone en moins de 20 ",
                      "minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to indoor sports activities"), 
          fr = paste0("Accessibilité aux activités sportives intérieures")) |> 
  add_row(en = paste0("Accessibility to health and social services facilities ",
                      "(Cycling)"), 
          fr = paste0("Accessibilité aux équipements de santé et de services s",
                      "ociaux (Vélo)")) |> 
  add_row(en = paste0("Health and social services facilities"), 
          fr = paste0("Équipements de santé et de services sociaux")) |> 
  add_row(en = paste0("the average number of health and social services facili",
                      "ties a resident of the area can reach within a 20-minut",
                      "e cycling time"), 
          fr = paste0("le nombre moyen d'équipements de santé et de services s",
                      "ociaux accessibles par un résident moyen de la zone en ",
                      "moins de 20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to health and social services facilities"), 
          fr = paste0("Accessibilité aux équipements de santé et de services s",
                      "ociaux")) |> 
  add_row(en = paste0("Accessibility to commercial zones (Cycling)"), 
          fr = paste0("Accessibilité aux zones commerciales (Vélo)")) |> 
  add_row(en = paste0("Commercial zones"), 
          fr = paste0("Zones commerciales")) |> 
  add_row(en = paste0("the average number of commercial zones a resident of th",
                      "e area can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de zones commerciales accessibles par u",
                      "n résident moyen de la zone en moins de 20 minutes à vé",
                      "lo")) |> 
  add_row(en = paste0("Accessibility to commercial zones"), 
          fr = paste0("Accessibilité aux zones commerciales")) |> 
  add_row(en = paste0("Accessibility to food stores (Cycling)"), 
          fr = paste0("Accessibilité aux commerces alimentaires (Vélo)")) |> 
  add_row(en = paste0("Food stores"), 
          fr = paste0("Commerces alimentaires")) |> 
  add_row(en = paste0("the average number of food stores a resident of the are",
                      "a can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de commerces alimentaires accessibles p",
                      "ar un résident moyen de la zone en moins de 20 minutes ",
                      "à vélo")) |> 
  add_row(en = paste0("Accessibility to food stores"), 
          fr = paste0("Accessibilité aux commerces alimentaires")) |> 
  add_row(en = paste0("Accessibility to pharmacies (Cycling)"), 
          fr = paste0("Accessibilité aux pharmacies (Vélo)")) |> 
  add_row(en = paste0("Pharmacies"), 
          fr = paste0("Pharmacies")) |> 
  add_row(en = paste0("the average number of pharmacies a resident of the area",
                      " can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de pharmacies accessibles par un réside",
                      "nt moyen de la zone en moins de 20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to pharmacies"), 
          fr = paste0("Accessibilité aux pharmacies")) |> 
  add_row(en = paste0("Accessibility to laundromats (Cycling)"), 
          fr = paste0("Accessibilité aux buanderies (Vélo)")) |> 
  add_row(en = paste0("Laundromats"), 
          fr = paste0("Buanderies")) |> 
  add_row(en = paste0("the average number of laundromats a resident of the are",
                      "a can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de buanderies accessibles par un réside",
                      "nt moyen de la zone en moins de 20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to laundromats"), 
          fr = paste0("Accessibilité aux buanderies")) |> 
  add_row(en = paste0("Accessibility to neighbourhood parks (Cycling)"), 
          fr = paste0("Accessibilité aux parcs de quartier (Vélo)")) |> 
  add_row(en = paste0("Neighbourhood parks"), 
          fr = paste0("Parcs de quartier")) |> 
  add_row(en = paste0("the average number of neighbourhood parks a resident of",
                      " the area can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de parcs de quartier accessibles par un",
                      " résident moyen de la zone en moins de 20 minutes à vél",
                      "o")) |> 
  add_row(en = paste0("Accessibility to neighbourhood parks"), 
          fr = paste0("Accessibilité aux parcs de quartier")) |> 
  add_row(en = paste0("Accessibility to big parks (Cycling)"), 
          fr = paste0("Accessibilité aux grands parcs (Vélo)")) |> 
  add_row(en = paste0("Big parks"), 
          fr = paste0("Grands parcs")) |> 
  add_row(en = paste0("the average number of big parks a resident of the area ",
                      "can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de grands parcs accessibles par un rési",
                      "dent moyen de la zone en moins de 20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to big parks"), 
          fr = paste0("Accessibilité aux grands parcs")) |> 
  add_row(en = paste0("Accessibility to playgrounds (Cycling)"), 
          fr = paste0("Accessibilité aux aires de jeux (Vélo)")) |> 
  add_row(en = paste0("Playgrounds"), 
          fr = paste0("Aires de jeux")) |> 
  add_row(en = paste0("the average number of playgrounds a resident of the are",
                      "a can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen d'aires de jeux accessibles par un rési",
                      "dent moyen de la zone en moins de 20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to playgrounds"), 
          fr = paste0("Accessibilité aux aires de jeux")) |> 
  add_row(en = paste0("Accessibility to cemeteries (Cycling)"), 
          fr = paste0("Accessibilité aux cimetières (Vélo)")) |> 
  add_row(en = paste0("Cemeteries"), 
          fr = paste0("Cimetières")) |> 
  add_row(en = paste0("the average number of cemeteries a resident of the area",
                      " can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen de cimetières accessibles par un réside",
                      "nt moyen de la zone en moins de 20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to cemeteries"), 
          fr = paste0("Accessibilité aux cimetières")) |> 
  add_row(en = paste0("Accessibility to nature facilities (Cycling)"), 
          fr = paste0("Accessibilité aux activités nature (Vélo)")) |> 
  add_row(en = paste0("Nature facilities"), 
          fr = paste0("Activités nature")) |> 
  add_row(en = paste0("the average number of nature facilities a resident of t",
                      "he area can reach within a 20-minute cycling time"), 
          fr = paste0("le nombre moyen d'activités nature accessibles par un r",
                      "ésident moyen de la zone en moins de 20 minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to nature facilities"), 
          fr = paste0("Accessibilité aux activités nature")) |> 
  add_row(en = paste0("Accessibility to outdoor water activities (Cycling)"), 
          fr = paste0("Accessibilité aux activités extérieures aquatiques (Vél",
                      "o)")) |> 
  add_row(en = paste0("Outdoor water activities"), 
          fr = paste0("Activités extérieures aquatiques")) |> 
  add_row(en = paste0("the average number of outdoor water activities a reside",
                      "nt of the area can reach within a 20-minute cycling ti",
                      "me"), 
          fr = paste0("le nombre moyen d'activités extérieures aquatiques acce",
                      "ssibles par un résident moyen de la zone en moins de 20",
                      " minutes à vélo")) |> 
  add_row(en = paste0("Accessibility to outdoor water activities"), 
          fr = paste0("Accessibilité aux activités extérieures aquatiques")) |> 
  add_row(en = paste0("Accessibility to outdoor winter activities (Cycling)"), 
          fr = paste0("Accessibilité aux activités extérieures d'hiver (Vélo)")) |> 
  add_row(en = paste0("Outdoor winter activities"), 
          fr = paste0("Activités extérieures d'hiver")) |> 
  add_row(en = paste0("the average number of outdoor winter activities a resid",
                      "ent of the area can reach within a 20-minute cycling t",
                      "ime"), 
          fr = paste0("le nombre moyen d'activités extérieures d'hiver accessi",
                      "bles par un résident moyen de la zone en moins de 20 mi",
                      "nutes à vélo")) |> 
  add_row(en = paste0("Accessibility to outdoor winter activities"), 
          fr = paste0("Accessibilité aux activités extérieures d'hiver")) |> 
  add_row(en = paste0("Accessibility to outdoor summer activities (Cycling)"), 
          fr = paste0("Accessibilité aux activités extérieures d'été (Vélo)")) |> 
  add_row(en = paste0("Outdoor summer activities"), 
          fr = paste0("Activités extérieures d'été")) |> 
  add_row(en = paste0("the average number of outdoor summer activities a resid",
                      "ent of the area can reach within a 20-minute cycling t",
                      "ime"), 
          fr = paste0("le nombre moyen d'activités extérieures d'été accessibl",
                      "es par un résident moyen de la zone en moins de 20 minu",
                      "tes à vélo")) |> 
  add_row(en = paste0("Accessibility to outdoor summer activities"), 
          fr = paste0("Accessibilité aux activités extérieures d'été")) |> 
  add_row(en = paste0("Accessibility to cultural facilities (Walk)"), 
          fr = paste0("Accessibilité aux activités culturelles (Marche)")) |> 
  add_row(en = paste0("Cultural facilities"), 
          fr = paste0("Activités culturelles")) |> 
  add_row(en = paste0("the average number of cultural facilities a resident of",
                      " the area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'activités culturelles accessibles par",
                      " un résident moyen de la zone en moins de 15 minutes à ",
                      "la marche")) |> 
  add_row(en = paste0("Accessibility to cultural facilities"), 
          fr = paste0("Accessibilité aux activités culturelles")) |> 
  add_row(en = paste0("Accessibility to daycares (Walk)"), 
          fr = paste0("Accessibilité aux garderies et CPE (Marche)")) |> 
  add_row(en = paste0("Daycares"), 
          fr = paste0("Garderies et cpe")) |> 
  add_row(en = paste0("the average number of daycares a resident of the area c",
                      "an reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de garderies et CPE accessibles par un ",
                      "résident moyen de la zone en moins de 15 minutes à la m",
                      "arche")) |> 
  add_row(en = paste0("Accessibility to daycares"), 
          fr = paste0("Accessibilité aux garderies et CPE")) |> 
  add_row(en = paste0("Accessibility to preschools (Walk)"), 
          fr = paste0("Accessibilité aux services de maternelle 4 ans (Marche)",
                      "")) |> 
  add_row(en = paste0("Preschools"), 
          fr = paste0("Services de maternelle 4 ans")) |> 
  add_row(en = paste0("the average number of preschools a resident of the area",
                      " can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de services de maternelle 4 ans accessi",
                      "bles par un résident moyen de la zone en moins de 15 mi",
                      "nutes à la marche")) |> 
  add_row(en = paste0("Accessibility to preschools"), 
          fr = paste0("Accessibilité aux services de maternelle 4 ans")) |> 
  add_row(en = paste0("Accessibility to primary scools (Walk)"), 
          fr = paste0("Accessibilité aux établissements scolaires primaires (M",
                      "arche)")) |> 
  add_row(en = paste0("Primary scools"), 
          fr = paste0("Établissements scolaires primaires")) |> 
  add_row(en = paste0("the average number of primary scools a resident of the ",
                      "area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'établissements scolaires primaires ac",
                      "cessibles par un résident moyen de la zone en moins de ",
                      "15 minutes à la marche")) |> 
  add_row(en = paste0("Accessibility to primary scools"), 
          fr = paste0("Accessibilité aux établissements scolaires primaires")) |> 
  add_row(en = paste0("Accessibility to secondary schools (Walk)"), 
          fr = paste0("Accessibilité aux établissements scolaires secondaires ",
                      "(Marche)")) |> 
  add_row(en = paste0("Secondary schools"), 
          fr = paste0("Établissements scolaires secondaires")) |> 
  add_row(en = paste0("the average number of secondary schools a resident of t",
                      "he area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'établissements scolaires secondaires ",
                      "accessibles par un résident moyen de la zone en moins d",
                      "e 15 minutes à la marche")) |> 
  add_row(en = paste0("Accessibility to secondary schools"), 
          fr = paste0("Accessibilité aux établissements scolaires secondaires")) |> 
  add_row(en = paste0("Accessibility to postsecondary schools (Walk)"), 
          fr = paste0("Accessibilité aux établissements postsecondaires (March",
                      "e)")) |> 
  add_row(en = paste0("Postsecondary schools"), 
          fr = paste0("Établissements postsecondaires")) |> 
  add_row(en = paste0("the average number of postsecondary schools a resident ",
                      "of the area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'établissements postsecondaires access",
                      "ibles par un résident moyen de la zone en moins de 15 m",
                      "inutes à la marche")) |> 
  add_row(en = paste0("Accessibility to postsecondary schools"), 
          fr = paste0("Accessibilité aux établissements postsecondaires")) |> 
  add_row(en = paste0("Accessibility to indoor sports activities (Walk)"), 
          fr = paste0("Accessibilité aux activités sportives intérieures (Marc",
                      "he)")) |> 
  add_row(en = paste0("Indoor sports activities"), 
          fr = paste0("Activités sportives intérieures")) |> 
  add_row(en = paste0("the average number of indoor sports activities a reside",
                      "nt of the area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'activités sportives intérieures acces",
                      "sibles par un résident moyen de la zone en moins de 15 ",
                      "minutes à la marche")) |> 
  add_row(en = paste0("Accessibility to indoor sports activities"), 
          fr = paste0("Accessibilité aux activités sportives intérieures")) |> 
  add_row(en = paste0("Accessibility to health and social services facilities ",
                      "(Walk)"), 
          fr = paste0("Accessibilité aux équipements de santé et de services s",
                      "ociaux (Marche)")) |> 
  add_row(en = paste0("Health and social services facilities"), 
          fr = paste0("Équipements de santé et de services sociaux")) |> 
  add_row(en = paste0("the average number of health and social services facili",
                      "ties a resident of the area can reach within a 15-minut",
                      "e walk"), 
          fr = paste0("le nombre moyen d'équipements de santé et de services s",
                      "ociaux accessibles par un résident moyen de la zone en ",
                      "moins de 15 minutes à la marche")) |> 
  add_row(en = paste0("Accessibility to health and social services facilities"), 
          fr = paste0("Accessibilité aux équipements de santé et de services s",
                      "ociaux")) |> 
  add_row(en = paste0("Accessibility to commercial zones (Walk)"), 
          fr = paste0("Accessibilité aux zones commerciales (Marche)")) |> 
  add_row(en = paste0("Commercial zones"), 
          fr = paste0("Zones commerciales")) |> 
  add_row(en = paste0("the average number of commercial zones a resident of th",
                      "e area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de zones commerciales accessibles par u",
                      "n résident moyen de la zone en moins de 15 minutes à la",
                      " marche")) |> 
  add_row(en = paste0("Accessibility to commercial zones"), 
          fr = paste0("Accessibilité aux zones commerciales")) |> 
  add_row(en = paste0("Accessibility to food stores (Walk)"), 
          fr = paste0("Accessibilité aux commerces alimentaires (Marche)")) |> 
  add_row(en = paste0("Food stores"), 
          fr = paste0("Commerces alimentaires")) |> 
  add_row(en = paste0("the average number of food stores a resident of the are",
                      "a can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de commerces alimentaires accessibles p",
                      "ar un résident moyen de la zone en moins de 15 minutes ",
                      "à la marche")) |> 
  add_row(en = paste0("Accessibility to food stores"), 
          fr = paste0("Accessibilité aux commerces alimentaires")) |> 
  add_row(en = paste0("Accessibility to pharmacies (Walk)"), 
          fr = paste0("Accessibilité aux pharmacies (Marche)")) |> 
  add_row(en = paste0("Pharmacies"), 
          fr = paste0("Pharmacies")) |> 
  add_row(en = paste0("the average number of pharmacies a resident of the area",
                      " can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de pharmacies accessibles par un réside",
                      "nt moyen de la zone en moins de 15 minutes à la marche")) |> 
  add_row(en = paste0("Accessibility to pharmacies"), 
          fr = paste0("Accessibilité aux pharmacies")) |> 
  add_row(en = paste0("Accessibility to laundromats (Walk)"), 
          fr = paste0("Accessibilité aux buanderies (Marche)")) |> 
  add_row(en = paste0("Laundromats"), 
          fr = paste0("Buanderies")) |> 
  add_row(en = paste0("the average number of laundromats a resident of the are",
                      "a can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de buanderies accessibles par un réside",
                      "nt moyen de la zone en moins de 15 minutes à la marche")) |> 
  add_row(en = paste0("Accessibility to laundromats"), 
          fr = paste0("Accessibilité aux buanderies")) |> 
  add_row(en = paste0("Accessibility to neighbourhood parks (Walk)"), 
          fr = paste0("Accessibilité aux parcs de quartier (Marche)")) |> 
  add_row(en = paste0("Neighbourhood parks"), 
          fr = paste0("Parcs de quartier")) |> 
  add_row(en = paste0("the average number of neighbourhood parks a resident of",
                      " the area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de parcs de quartier accessibles par un",
                      " résident moyen de la zone en moins de 15 minutes à la ",
                      "marche")) |> 
  add_row(en = paste0("Accessibility to neighbourhood parks"), 
          fr = paste0("Accessibilité aux parcs de quartier")) |> 
  add_row(en = paste0("Accessibility to big parks (Walk)"), 
          fr = paste0("Accessibilité aux grands parcs (Marche)")) |> 
  add_row(en = paste0("Big parks"), 
          fr = paste0("Grands parcs")) |> 
  add_row(en = paste0("the average number of big parks a resident of the area ",
                      "can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de grands parcs accessibles par un rési",
                      "dent moyen de la zone en moins de 15 minutes à la march",
                      "e")) |> 
  add_row(en = paste0("Accessibility to big parks"), 
          fr = paste0("Accessibilité aux grands parcs")) |> 
  add_row(en = paste0("Accessibility to playgrounds (Walk)"), 
          fr = paste0("Accessibilité aux aires de jeux (Marche)")) |> 
  add_row(en = paste0("Playgrounds"), 
          fr = paste0("Aires de jeux")) |> 
  add_row(en = paste0("the average number of playgrounds a resident of the are",
                      "a can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'aires de jeux accessibles par un rési",
                      "dent moyen de la zone en moins de 15 minutes à la march",
                      "e")) |> 
  add_row(en = paste0("Accessibility to playgrounds"), 
          fr = paste0("Accessibilité aux aires de jeux")) |> 
  add_row(en = paste0("Accessibility to cemeteries (Walk)"), 
          fr = paste0("Accessibilité aux cimetières (Marche)")) |> 
  add_row(en = paste0("Cemeteries"), 
          fr = paste0("Cimetières")) |> 
  add_row(en = paste0("the average number of cemeteries a resident of the area",
                      " can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen de cimetières accessibles par un réside",
                      "nt moyen de la zone en moins de 15 minutes à la marche")) |> 
  add_row(en = paste0("Accessibility to cemeteries"), 
          fr = paste0("Accessibilité aux cimetières")) |> 
  add_row(en = paste0("Accessibility to nature facilities (Walk)"), 
          fr = paste0("Accessibilité aux activités nature (Marche)")) |> 
  add_row(en = paste0("Nature facilities"), 
          fr = paste0("Activités nature")) |> 
  add_row(en = paste0("the average number of nature facilities a resident of t",
                      "he area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'activités nature accessibles par un r",
                      "ésident moyen de la zone en moins de 15 minutes à la ma",
                      "rche")) |> 
  add_row(en = paste0("Accessibility to nature facilities"), 
          fr = paste0("Accessibilité aux activités nature")) |> 
  add_row(en = paste0("Accessibility to outdoor water activities (Walk)"), 
          fr = paste0("Accessibilité aux activités extérieures aquatiques (Mar",
                      "che)")) |> 
  add_row(en = paste0("Outdoor water activities"), 
          fr = paste0("Activités extérieures aquatiques")) |> 
  add_row(en = paste0("the average number of outdoor water activities a reside",
                      "nt of the area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'activités extérieures aquatiques acce",
                      "ssibles par un résident moyen de la zone en moins de 15",
                      " minutes à la marche")) |> 
  add_row(en = paste0("Accessibility to outdoor water activities"), 
          fr = paste0("Accessibilité aux activités extérieures aquatiques")) |> 
  add_row(en = paste0("Accessibility to outdoor winter activities (Walk)"), 
          fr = paste0("Accessibilité aux activités extérieures d'hiver (Marche",
                      ")")) |> 
  add_row(en = paste0("Outdoor winter activities"), 
          fr = paste0("Activités extérieures d'hiver")) |> 
  add_row(en = paste0("the average number of outdoor winter activities a resid",
                      "ent of the area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'activités extérieures d'hiver accessi",
                      "bles par un résident moyen de la zone en moins de 15 mi",
                      "nutes à la marche")) |> 
  add_row(en = paste0("Accessibility to outdoor winter activities"), 
          fr = paste0("Accessibilité aux activités extérieures d'hiver")) |> 
  add_row(en = paste0("Accessibility to outdoor summer activities (Walk)"), 
          fr = paste0("Accessibilité aux activités extérieures d'été (Marche)")) |> 
  add_row(en = paste0("Outdoor summer activities"), 
          fr = paste0("Activités extérieures d'été")) |> 
  add_row(en = paste0("the average number of outdoor summer activities a resid",
                      "ent of the area can reach within a 15-minute walk"), 
          fr = paste0("le nombre moyen d'activités extérieures d'été accessibl",
                      "es par un résident moyen de la zone en moins de 15 minu",
                      "tes à la marche")) |> 
  add_row(en = paste0("Accessibility to outdoor summer activities"), 
          fr = paste0("Accessibilité aux activités extérieures d'été")) |> 
  add_row(en = paste0("15-minute walking time"), 
          fr = paste0("15 minutes à la marche")) |> 
  add_row(en = paste0("20-minute cycling time"), 
          fr = paste0("20 minutes à vélo"))



