## ACCESSIBILITY VARIABLES TRANSLATION #########################################

translation_access <- 
  tibble(en = character(),
         fr = character()) |> 
  add_row(en = "Total",
          fr = "Total") |>
  add_row(en = "Amenity",
          fr = "Service") |>
  add_row(en = "Retail stores type",
          fr = "Type de magasins de détail") |> 
  add_row(en = "Finance establishment",
          fr = "Établissement financier") |> 
  add_row(en = "Food industry",
          fr = "Industrie alimentaire") |> 
  add_row(en = "Health care facility",
          fr = "Établissement de soins de santé") |> 
  add_row(en = "Educational establishment category",
          fr = "Catégorie d'établissement d'enseignement") |> 
  add_row(en = "Cultural facility",
          fr = "Établissement culturel") |> 
  add_row(en = stringr::str_to_sentence("General Merchandise Stores"),
          fr = "Magasins de marchandises générales") |>
  add_row(en = stringr::str_to_sentence("Apparel And Accessory Stores"),
          fr = "Magasins de vêtements et d'accessoires") |>
  add_row(en = stringr::str_to_sentence("Home Furniture, Furnishings, And Equipment Stores"),
          fr = "Magasins de meubles, d'ameublement et d'équipement pour la maison") |>
  add_row(en = stringr::str_to_sentence("Eating And Drinking Places"),
          fr = "Restaurants et bars") |>
  add_row(en = stringr::str_to_sentence("Miscellaneous Retail"),
          fr = "Commerce de détail divers") |>
  add_row(en = stringr::str_to_sentence("Retail Trade"),
          fr = "Commerce de détail") |>
  add_row(en = stringr::str_to_sentence("Depository Institutions"),
          fr = "Établissements de dépôt") |>
  add_row(en = stringr::str_to_sentence("Non-depository Credit Institutions"),
          fr = "Établissements de crédit non dépositaires") |>
  add_row(en = stringr::str_to_sentence("Financial Institutions"),
          fr = "Institutions financières") |>
  add_row(en = stringr::str_to_sentence("Grocery Stores"),
          fr = "Épiceries") |>
  add_row(en = stringr::str_to_sentence("Meat and Fish Markets"),
          fr = "Marchés de viande et de poisson") |>
  add_row(en = stringr::str_to_sentence("Fruit and Vegetable Markets"),
          fr = "Marchés de fruits et légumes") |>
  add_row(en = stringr::str_to_sentence("Dairy Products Stores"),
          fr = "Magasins de produits laitiers") |>
  add_row(en = stringr::str_to_sentence("Retail Bakeries"),
          fr = "Boulangeries de détail") |>
  add_row(en = stringr::str_to_sentence("Miscellaneous Food Stores"),
          fr = "Magasins d'alimentation divers") |>
  add_row(en = stringr::str_to_sentence("Food Stores"),
          fr = "Magasins d'alimentation") |>
  add_row(en = stringr::str_to_sentence("Dance Studios, Schools, And Halls"),
          fr = "Studios, écoles et salles de danse") |>
  add_row(en = stringr::str_to_sentence("Theatrical Producers"),
          fr = "Producteurs de cinéma") |>
  add_row(en = stringr::str_to_sentence("Bowling Centers"),
          fr = "Centres de bowling") |>
  add_row(en = stringr::str_to_sentence("Physical Fitness Facilities"),
          fr = "Centres de conditionnement physique") |>
  add_row(en = stringr::str_to_sentence("Miscellaneous Amusement And Recreation"),
          fr = "Amusement et loisirs divers") |>
  add_row(en = stringr::str_to_sentence("Recreation Services"),
          fr = "Services de loisirs") |>
  add_row(en = "Ambulatory health care services",
          fr = "Services de soins de santé ambulatoires") |>
  add_row(en = "Hospitals",
          fr = "Hôpitaux") |>
  add_row(en = "Nursing and residential care facilities",
          fr = "Établissements de soins infirmiers et résidentiels") |>
  add_row(en = "Healthcare facilities",
          fr = "Établissements de soins de santé") |>
  add_row(en = "Early childhood and/or kindergarten education",
          fr = "Éducation de la petite enfance et/ou de l'école maternelle") |>
  add_row(en = "Elementary education",
          fr = "Écoles élémentaires") |>
  add_row(en = "Secondary education",
          fr = "Écoles secondaires") |>
  add_row(en = "Post-secondary education",
          fr = "Écoles post-secondaires") |>
  add_row(en = "Educational facilities",
          fr = "Établissements d'enseignement") |>
  add_row(en = "Art or cultural centre",
          fr = "Centre d'art ou de culture") |>
  add_row(en = "Gallery",
          fr = "Galerie") |>
  add_row(en = "Heritage or historic site",
          fr = "Patrimoine ou site historique") |>
  add_row(en = "Library or archive",
          fr = "Bibliothèque ou archives") |>
  add_row(en = "Museum",
          fr = "Musée") |>
  add_row(en = "Theatre/performance and concert hall",
          fr = "Théâtre/performance et salle de concert") |>
  add_row(en = "Miscellaneous cultural facilities",
          fr = "Équipements culturels divers") |>
  add_row(en = "Cultural facilities",
          fr = "Équipements culturels") |> 
  add_row(en = "General",
          fr = "Général") |>
  add_row(en = "Apparel",
          fr = "Vêtements") |>
  add_row(en = "Furniture",
          fr = "Furniture") |>
  add_row(en = "Eating",
          fr = "Nourriture") |>
  add_row(en = "Retail",
          fr = "Détail") |>
  add_row(en = "Retail",
          fr = "Détail") |>
  add_row(en = "Depository",
          fr = "Dépôt") |>
  add_row(en = "Credit",
          fr = "Crédit") |>
  add_row(en = "Finance",
          fr = "Finances") |>
  add_row(en = "Groceries",
          fr = "Épicerie") |>
  add_row(en = "Meat",
          fr = "Viande") |>
  add_row(en = "Fruit/Veg.",
          fr = "Fruits/lég.") |>
  add_row(en = "Dairy",
          fr = "Laitiers") |>
  add_row(en = "Bakeries",
          fr = "Boulangeries") |>
  add_row(en = "Food",
          fr = "Alimentation") |>
  add_row(en = "Food",
          fr = "Alimentation") |>
  add_row(en = "Ambulatory",
          fr = "Ambulatoire") |>
  add_row(en = "Hospitals",
          fr = "Hôpitaux") |>
  add_row(en = "Nursing",
          fr = "Soins infirmiers") |>
  add_row(en = "Healthcare",
          fr = "Soins de santé") |>
  add_row(en = "Kindergarten",
          fr = "Garderies") |>
  add_row(en = "Elementary",
          fr = "Élémentaire") |>
  add_row(en = "Secondary",
          fr = "Secondaire") |>
  add_row(en = "Post-secondary",
          fr = "Post-secondaire") |>
  add_row(en = "Educational",
          fr = "Éducation") |>
  add_row(en = "Art/cultural",
          fr = "Art/culturel") |>
  add_row(en = "Gallery",
          fr = "Galerie") |>
  add_row(en = "Heritage",
          fr = "Patrimoine") |>
  add_row(en = "Library",
          fr = "Bibliothèque") |>
  add_row(en = "Museum",
          fr = "Musée") |>
  add_row(en = "Theatre",
          fr = "Théâtre") |>
  add_row(en = "Miscellaneous",
          fr = "Divers") |>
  add_row(en = "Cultural",
          fr = "Culturel") |>
  add_row(en = "places en garderie",
          fr = "daycare spots") |> 
  add_row(en = "Daycare spots",
          fr = "Places en garderie") |> 
  add_row(en = "Daycare",
          fr = "Garderie") |> 
  add_row(en = "Dance Studios",
          fr = "Studios de danse") |>
  add_row(en = "Bowling",
          fr = "Bowling") |>
  add_row(en = "Fitness",
          fr = "Fitness") |>
  add_row(en = "Recreation",
          fr = "Loisir") |> 
  
  add_row(en = "Mode of transport",
          fr = "Mode de transport") |> 
  add_row(en = "Transportation time",
          fr = "Temps de transport") |> 
  add_row(en = "Timing",
          fr = "Horaire") |> 
  
  add_row(en = "Weekend traffic off-peak",
          fr = "Week-end hors pointe") |>
  add_row(en = "Weekend traffic peak",
          fr = "Week-end à l'heure de pointe") |>
  add_row(en = "Weekday night",
          fr = "Nuit en semaine") |>
  add_row(en = "Weekend night",
          fr = "Nuit de week-end") |>
  add_row(en = "Weekday traffic off-peak",
          fr = "Jour de semaine hors pointe") |>
  add_row(en = "Weekday traffic peak",
          fr = "Jour de semaine à l'heure de pointe") |>
  
  add_row(en = "Access to retail stores",
          fr = "Accès aux magasins de détail") |> 
  add_row(en = "Access to finance establishments",
          fr = "Accès aux établissements financiers") |> 
  add_row(en = "Access to food distributors",
          fr = "Accès aux distributeurs alimentaires") |> 
  add_row(en = "Access to healthcare facilities",
          fr = "Accès aux établissements de soins de santé") |> 
  add_row(en = "Access to schools",
          fr = "Accès aux écoles") |> 
  add_row(en = "Access to cultural facilities",
          fr = "Accès aux établissements culturels") |> 
  add_row(en = "Access to daycare spots",
          fr = "Accès aux places en garderie") |> 
  add_row(en = "Access to daycare spots",
          fr = "Accès aux places en garderie") |> 
  add_row(en = "Access to recreational services",
          fr = "Accès aux services de loisirs") |> 
  
  add_row(en = "Walking",
          fr = "À pieds") |> 
  add_row(en = "Bicycle",
          fr = "Vélo") |> 
  add_row(en = "Car",
          fr = "Auto") |> 
  add_row(en = "Public transit",
          fr = "Transport en commun")


# Go over every possible variable
vars <- variables$var_code[grepl("^access", variables$var_code)]

additional_vars <- lapply(vars, \(var) {

  dict <- cc.data::accessibility_point_dict
  dict <- dict[sapply(dict$var, grepl, var), ]
  if (nrow(dict) == 0) {
    if (grepl("daycarespots", var)) {
      dict <- tibble::tibble(title = "Daycare spots",
                             short = "Daycare")
    }
  }
  title <- translation_access$fr[tolower(translation_access$en) == tolower(dict$title)]
  title <- unique(title)
  short <- translation_access$fr[tolower(translation_access$en) == tolower(dict$short)]
  short <- unique(short)
  
  mode <- (\(x) {
    if (grepl("_car_", var)) return("en auto")
    if (grepl("_foot_", var)) return("à pieds")
    if (grepl("_bicycle_", var)) return("à vélo")
    if (grepl("_transit_opwe_", var)) return("en transports en commun les jours de week-end hors pointe")
    if (grepl("_transit_pwe_", var)) return("en transports en commun les jours de week-end à l'heure de pointe")
    if (grepl("_transit_nwd_", var)) return("en transports en commun les jours de semaine la nuit")
    if (grepl("_transit_nwe_", var)) return("en transports en commun les jours de week-end la nuit")
    if (grepl("_transit_opwd_", var)) return("en transports en commun les jours de semaine hors pointe")
    if (grepl("_transit_pwd_", var)) return("en transports en commun les jours de semaine à l'heure de pointe")
  })()
  
  time <- gsub("_", "", stringr::str_extract(var, "_\\d*_"))
  
  var_title <- stringr::str_to_sentence(paste0(title, " accessibles ", mode))
  var_short <- stringr::str_to_sentence(short)
  explanation <- paste0(
    "le nombre, en moyenne, de/d' ", tolower(title),
    " qu'un habitant peut atteindre en ", time, " minutes ", mode
  )
  
  explanation <- if (grepl("^(a|e|i|o|u|h)", tolower(title))) {
    gsub("de/d' ", "d'", explanation)
  } else {
    gsub("de/d' ", "de ", explanation)
  }
  
  explanation_nodet <- gsub("^le ", "", explanation)
  
  exp_q5 <- paste0(
    "le résident moyen a accès à _X_ ", tolower(title), " en ", time,
    " minutes ", mode
  )
  
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

translation_access <- rbind(translation_access, additional_vars)
