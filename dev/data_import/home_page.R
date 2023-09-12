## HOME PAGE TIBBLES ###########################################################

home_page <- function(data_path = "data/", stories, translation_df) {
  ### READ ?cc.landing::landing_input DOCUMENTATION TO CONSTRUCT CORRECTLY
  # EACH OF THESE OBJECTS.

  # Path to the top-left corner SVG image of  -------------------------------
  c_city_svg <- "www/landing/c-montreal.svg"


  # Tibble for the news section ---------------------------------------------
  news_cards <- tibble::tibble(
    id = c("census", "centraide", "alp"),
    icon = c("demographics", "urban", "health"),
    title_en = c("2021 Census", "Centraide x Curbcut", "Active Living Potential"),
    title_fr = c("Recensement de 2021", "Centraide x Curbcut", "Potentiel de vie active"),
    text_en = c(
      "We’ve added new data to the site! 2021 Census data is now available on all pages.",
      "In a novel collaboration, Centraide of Greater Montreal is partnering with Curbcut on a series of housing maps. Centraide is using its social expertise and data to help target and interpret housing issues, a decisive factor in poverty and social exclusion.",
      "Curbcut has developed its own Active Living Potential index. This index quantifies which areas provide walkable environments to their residents based on street connectivity, building density and points of interest. We developed the index with out internal travel time matrix dataset which uses a 15-minute walk buffer on the street network."
    ),
    text_fr = c(
      "Nous avons ajouté de nouvelles données au site ! Les données du recensement de 2021 sont maintenant disponibles sur toutes les pages.",
      "Dans une collaboration inédite, Centraide du Grand Montréal s’associe à Curbcut à travers une série de cartes dédiées au logement. Centraide met son expertise sociale et ses données au profit de la plateforme pour mieux cibler et interpréter les enjeux liés au logement, qui est un facteur déterminant sur la pauvreté et l’exclusion sociale.",
      "Curbcut a développé son propre indice de potentiel de vie active. Cet indice quantifie les zones qui offrent des environnements propices à la marche à leurs résidents en fonction de la connectivité des rues, de la densité des bâtiments et des points d'intérêt. Nous avons développé cet indice à l'aide de notre matrice interne de données sur les temps de déplacement, qui utilise un tampon de 15 minutes de marche sur le réseau de rues."
    )
  )


  # Tibble for the discover section -----------------------------------------

#   discover_cards <- tibble::tibble(
#     id = c("metro", "alp", "gentrification", "safety"),
#     img = c(
#       "www/landing/discover/metro_evolution.png",
#       "www/landing/discover/ALP_Discover.png",
#       "www/landing/discover/mont_royal.png",
#       "www/landing/discover/RoadSafety_Discover.png"
#     ),
#     theme = c("urban", "health", "urban", "transport"),
#     en = c("The Evolution of the Metro", 
#            "ALP and people who drive to work", 
#            "The Mount-Royal", 
#            "Car crashes between 2017 and 2021"),
#     fr = c("L'Évolution du Métro", 
#            "Le PVA et les personnes qui se rendent au travail en voiture", 
#            "Le Mont-Royal", 
#            "Les collisions de voiture entre 2017 et 2021")
#   )
#   discover_cards$preview_en <- paste("English preview for", discover_cards$en)
#   discover_cards$preview_fr <- paste("Preview français pour", discover_cards$fr)
  
  discover_cards <- tibble::tibble(id = character(), 
                                   img = character(),
                                   theme = character(),
                                   en = character(),
                                   fr = character(),
                                   preview_en = character(),
                                   preview_fr = character())
  
  discover_cards <- 
    discover_cards |> 
    tibble::add_row(id = "climate_risk",
                    img = "climate_risk.png",
                    theme = "climate",
                    en = "Climate change risk",
                    fr = "Risque climatique",
                    preview_en = "Climate change will have increasingly negative impacts on communities.",
                    preview_fr = "Le changement climatique aura des effets de plus en plus négatifs sur les communautés.") |> 
    tibble::add_row(id = "land_surface",
                    img = "LST.png",
                    theme = "climate",
                    en = "Land surface temperature",
                    fr = "Température au sol",
                    preview_en = "LST is a crucial indicator of urban heat islands and ecological balance within a region.",
                    preview_fr = "La TAS est un indicateur crucial des îlots de chaleur urbains et de l'équilibre écologique au sein d'une région.") |> 
    tibble::add_row(id = "greenness",
                    img = "greenness.png",
                    theme = "ecology",
                    en = "Greenness",
                    fr = "Verdure",
                    preview_en = "The Normalized Difference Vegetation Index (NDVI) is a vital measurement for understanding the presence and intensity of vegetation in an area.",
                    preview_fr = "L'indice de végétation par différence normalisée (IVDN) est une mesure essentielle pour comprendre la présence et l'intensité de la végétation dans une zone.") |> 
    tibble::add_row(id = "green_alleys",
                    img = "green_alleys_map.png",
                    theme = "ecology",
                    en = "Green alleys",
                    fr = "Ruelles vertes",
                    preview_en = "Public spaces that have been transformed by residents for their own activities.",
                    preview_fr = "Public spaces that have been transformed by residents for their own activities.") |> 
    tibble::add_row(id = "natural_infrastructure",
                    img = "natural_infrastructure.png",
                    theme = "ecology",
                    en = "Natural infrastructure",
                    fr = "Infrastructures naturelles",
                    preview_en = "Natural ecosystems are necessary for our cities, they contribute to well-being, quality of life and public health.",
                    preview_fr = "Les écosystèmes naturels sont nécessaires à nos villes, ils contribuent au bien-être, à la qualité de vie et à la santé publique.") |> 
    tibble::add_row(id = "housing_system",
                    img = "housing_system.png",
                    theme = "housing",
                    en = "The housing system",
                    fr = "Système de logement",
                    preview_en = "Housing is at the centre of our lives. Our ability to find affordable, adequate, and healthy accommodations profoundly affects our life chances.",
                    preview_fr = "Le logement est au cœur de nos vies. Notre capacité à trouver un logement abordable, adéquat et sain affecte profondément nos chances dans la vie.")
  
  if (length(unique(discover_cards$id)) != nrow(discover_cards)) {
    stop("Discover cards do not have unique ids")
  }
  
  
  # Stories formatting for the discover_cards
  disc_stories <- stories[c("name_id", "short_title", "preview_en", "preview_fr")]
  names(disc_stories) <- c("id", "en", "preview_en", "preview_fr")
  disc_stories$img <- sprintf("%s.png", disc_stories$id)
  disc_stories$theme <- "urban"
  disc_stories$fr <- sapply(disc_stories$en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)

  # Bind 
  discover_cards <- rbind(discover_cards, disc_stories)
  
  # Filter out missing photos and warn!
  present_img <- discover_cards$img %in% list.files("www/landing/discover/")
  missing_img <- discover_cards[!present_img, ]
  if (nrow(missing_img) > 0){
    warning(paste0("Missing images for ", missing_img$id, "\n"))
  }
  
  discover_cards <- discover_cards[present_img, ]
  discover_cards$img <- paste0("www/landing/discover/", discover_cards$img)

  # Tibble for team members -------------------------------------------------
  team_cards <- tibble::tibble(
    id = c("davidw", "kevinm", "maxbdb", "dominiqueb"),
    img = c(
      "www/landing/team/david_wachsmuth.jpeg",
      "www/landing/team/kevin_manaugh.jpg",
      "www/landing/team/maxime_belanger_de_blois.jpg",
      "www/landing/team/dominique_boulet.jpg"
    ),
    name = c("David Wachsmuth", "Kevin Manaugh", "Maxime Bélanger De Blois", "Dominique Boulet"),
    role_en = c("Co-founder & Co-CEO", "Co-founder & Co-CEO", "Head of Technology and Data", "Qualitative Research Lead"),
    role_fr = c("Co-fondateur et co-PDG", "Co-fondateur et co-PDG", "Responsable technologie et données", "Responsable de la recherche qualitative"),
    # bio_en = c("David is one of the world’s leading experts on the impacts of short-term rental platforms, such as Airbnb, on cities around the world and consults widely with municipalities and community organizations on designing appropriate regulations. In addition to his work at Curbcut, David is the Canada Research Chair in Urban Governance at McGill University, where he is also an Associate Professor in the School of Urban Planning.",
    #            "Kevin is one of the leading experts on the intersection between urban transport systems and social and environmental justice. In addition to his work at Curbcut, Kevin is also an associate professor at McGill University jointly appointed in the Department of Geography and the Bieler School of Environment.",
    #            "Maxime is a skilled, resourceful and forward-thinking data scientist, adept at developing and transforming intricate datasets into actionable intelligence. With a master's degree in Urban Planning from McGill University, his extensive understanding of data analysis and geovisualization enables him to extract valuable insights and provide innovative solutions.",
    #            "Dominique is driven to create qualitative work that complements quantitative information. She has a master’s degree in Urban Planning from McGill University and a master’s degree in Anthropology from Aarhus University, Copenhagen."),
    # bio_fr = c("David est l'un des plus grands experts mondiaux sur les impacts des plateformes de location à court terme, telles que Airbnb, sur les villes du monde entier et consulte largement les municipalités et les organisations communautaires sur la conception de réglementations appropriées. En plus de son travail chez Curbcut, David est titulaire de la Chaire de recherche du Canada en gouvernance urbaine à l'Université McGill, où il est également professeur associé à l'École d'urbanisme.",
    #            "Kevin est l'un des principaux experts de l'intersection entre les systèmes de transport urbain et la justice sociale et environnementale. En plus de son travail chez Curbcut, Kevin est également professeur associé à l'Université McGill, nommé conjointement au département de géographie et à l'école d'environnement Bieler.",
    #            "Maxime est un scientifique de données habile, ingénieux et avant-gardiste, capable de concevoir et de transformer des ensembles de données complexes en renseignements exploitables. Diplômé d'une maîtrise en urbanisme de l'Université McGill, sa connaissance approfondie de l'analyse des données et de la géovisualisation lui permet d'extraire des informations précieuses et de proposer des solutions innovantes.",
    #            "Dominique est motivée par la nécessité de produire des études qualitatives qui complètent les informations quantitatives. Elle est titulaire d'une maîtrise en urbanisme de l'Université McGill et d'une maîtrise en anthropologie de l'Université d'Aarhus, à Copenhague."),
    theme = c("housing", "transport", "health", "urban")
  )


  # Character vector for contributors ---------------------------------------
  contributors <- c(
    "Cloé St-Hilaire",
    "Emma Ezvan",
    "Daniela Rodriguez",
    "Connor Cordingley",
    "Robin Basalaev-Binder",
    "Josh Medicoff",
    "Philip Bligh",
    "Lauren Rosenthal"
  )


  # Tibble for collaborators ------------------------------------------------
  collabs <- tibble::tibble(
    id = c("MSSI", "centraide"),
    img = c(
      "www/landing/collab/mcgill-logo.png",
      "www/landing/collab/centraide-logo.png"
    ),
    name = c("The McGill Sustainability Systems Initiative", "Centraide")
  )


  # Save home page information as qsm ---------------------------------------

  qs::qsavem(c_city_svg, news_cards, discover_cards,
             team_cards, contributors, collabs,
             file = paste0(data_path, "home_page.qsm")
  )
}
