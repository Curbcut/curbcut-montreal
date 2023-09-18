## HOME PAGE TIBBLES ###########################################################

home_page <- function(modules, stories, translation_df, data_path = "data/") {
  ### READ ?cc.landing::landing_input DOCUMENTATION TO CONSTRUCT CORRECTLY
  # EACH OF THESE OBJECTS.
  
  # Encode images to base64 for the input
  base64 <- function(x) {
    # Read the JPG image as raw binary data
    image_data <- readBin(x, "raw", file.info(x)$size)
    
    # Encode the image data to base64
    paste0("data:image/jpeg;base64,", base64enc::base64encode(image_data))
  }
  

  # Path to the top-left corner SVG image of  -------------------------------
  c_city_svg <- "www/landing/c-montreal.svg"


  # Tibble for the news section ---------------------------------------------

  news_cards <- tibble::tibble(id = character(),
                               icon = character(),
                               title_en = character(),
                               title_fr = character(),
                               text_en = character(),
                               text_fr = character(),
                               link = character())
  
  news_cards <- 
    news_cards |> 
    tibble::add_row(id = "curbcut5a7", 
                    icon = "transport", 
                    title_en = "Curbcut 5@7", 
                    title_fr = "5@7 Curbcut", 
                    text_en = paste0(
                      "Join us in celebrating Curbcut’s latest update! We’re hosting a 5@7 ev",
                      "ent at the McGill Engine Centre on October 2nd. Don’t miss this opport",
                      "unity to explore our innovative new features and join a discussion on ",
                      "how Curbcut is shaping the future of urban sustainability. See you the",
                      "re!"
                    ), 
                    text_fr = paste0(
                      "Rejoignez-nous pour célébrer la toute dernière mise à jour de Curbcut ",
                      "! Nous organisons un 5@7 au Centre d'Innovation McGill le 2 octobre. N",
                      "e manquez pas cette occasion de découvrir nos nouvelles fonctionnalité",
                      "s innovantes et de participer à des discussions sur la ma",
                      "nière dont Curbcut façonne l'avenir de la durabilité urbaine. À très b",
                      "ientôt !"
                    ),
                    link = "https://www.eventbrite.ca/e/curbcut-57-tickets-705402857937") |> 
    tibble::add_row(id = "greenness", 
                    icon = "ecology", 
                    title_en = "Explore urban greenery", 
                    title_fr = "Explorez la verdure urbaine", 
                    text_en = paste0(
                      "Curbcut unveils its cutting-edge page focused on Normalized Difference ",
                      "Vegetation Index (NDVI) analytics for in-depth vegetation analysis. ",
                      "Leveraging advanced satellite data, the platform offers a c",
                      "omprehensive look at plant health and density in urban areas. Through ",
                      "precise calculations and quality filtering, we provide actionable insi",
                      "ghts for urban planning and environmental conservation. This becomes a",
                      " crucial asset for stakeholders in urban sustainability and environmen",
                      "tal justice."
                    ), 
                    text_fr = paste0(
                      "Curbcut dévoile sa nouvelle page innovante axée sur l'analyse approfon",
                      "die de l'Indice de Différence de Végétation Normalisée (NDVI). En expl",
                      "oitant des données satellitaires avancées, la plateforme offre un aper",
                      "çu complet de la santé et de la densité des plantes en milieu urbain. ",
                      "Grâce à des calculs précis et à un filtrage de qualité, nous offrons d",
                      "es renseignements concrets utiles pour l'aménagement urbain et la prés",
                      "ervation de l'environnement. Cet outil devient un atout essentiel pour",
                      " les acteurs de la durabilité urbaine et de la justice environnemental",
                      "e."
                    ),
                    link = "ndvi") |> 
    tibble::add_row(id = "lst", 
                    icon = "climat", 
                    title_en = "Land surface temperature", 
                    title_fr = "Température au sol", 
                    text_en = paste0(
                      "Unveiling our latest Land Surface Temperature (LST) analytics! Our new",
                      " LST page provides accurate, three-year mean temperature estimates. A ",
                      "must-see for researchers and policymakers in urban sustainability."
                    ), 
                    text_fr = paste0(
                      "Découvrez nos dernières analyses sur la température au sol ! Notre nou",
                      "velle page offre des estimations précises de la température moyenne su",
                      "r une période de trois ans. Un passage obligé pour les chercheurs et l",
                      "es décideurs en durabilité urbaine."
                    ),
                    link = "lst") |> 
    tibble::add_row(id = "alp", 
                    icon = "health", 
                    title_en = "Active living potential", 
                    title_fr = "Potentiel de vie active", 
                    text_en = paste0(
                      "Curbcut has developed its own Active Living Potential index. This inde",
                      "x quantifies which areas provide walkable environments to their reside",
                      "nts based on street connectivity, building density and points of inter",
                      "est. We developed the index with out internal travel time matrix datas",
                      "et which uses a 15-minute walk buffer on the street network."
                    ), 
                    text_fr = paste0(
                      "Curbcut a développé son propre indice de potentiel de vie active. Cet ",
                      "indice quantifie les zones qui offrent des environnements propices à l",
                      "a marche à leurs résidents en fonction de la connectivité des rues, de",
                      " la densité des bâtiments et des points d'intérêt. Nous avons développ",
                      "é cet indice à l'aide de notre matrice interne de données sur les temp",
                      "s de déplacement, qui utilise un tampon de 15 minutes de marche sur le",
                      " réseau de rues."
                    ),
                    link = "alp") |> 
    tibble::add_row(id = "census", 
                    icon = "demographics", 
                    title_en = "2021 Census", 
                    title_fr = "Recensement de 2021", 
                    text_en = paste0(
                      "We’ve added new data to the site! 2021 Census data is now available on",
                      " all pages."
                    ), 
                    text_fr = paste0(
                      "Nous avons ajouté de nouvelles données au site ! Les données du recens",
                      "ement de 2021 sont maintenant disponibles sur toutes les pages."
                    ),
                    link = NA)


  # Tibble for the discover section -----------------------------------------

  # Function to remove all HTML tags from a given string
  remove_html_tags <- function(input_vector) {
    # Use gsub to replace all HTML tags with an empty string
    output_vector <- gsub("<[^>]*>", "", input_vector)
    return(output_vector)
  }
  
  # Pages from the modules
  disc_modules <- modules[c("id", "title_text_title", "title_text_main", "theme")]
  names(disc_modules) <- c("id", "en", "preview_en", "theme")
  disc_modules$img <- sprintf("%s.png", disc_modules$id)
  disc_modules$theme <- gsub(" .*", "", disc_modules$theme) |> tolower()
  disc_modules$preview_fr <- sapply(disc_modules$preview_en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)
  disc_modules$preview_fr <- remove_html_tags(disc_modules$preview_fr)
  disc_modules$preview_en <- remove_html_tags(disc_modules$preview_en)
  disc_modules$fr <- sapply(disc_modules$en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)
  disc_modules <- disc_modules[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr")]
  disc_modules$type <- "page"
  disc_modules$select_id <- NA
  
  # Stories formatting for the discover_cards
  disc_stories <- stories[c("name_id", "short_title", "preview_en", "preview_fr", "ID")]
  names(disc_stories) <- c("id", "en", "preview_en", "preview_fr", "select_id")
  disc_stories$img <- sprintf("%s.png", disc_stories$id)
  disc_stories$theme <- "urban"
  disc_stories$fr <- sapply(disc_stories$en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)
  disc_stories$type <- "stories"
  # disc_stories <- disc_stories[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr", "type", "select_id")]

  # Bindthe modules with the stories and the DYK
  discover_cards <- rbind(disc_modules, disc_stories)
  
  # Filter out missing photos and warn!
  present_img <- discover_cards$img %in% list.files("www/landing/discover/")
  missing_img <- discover_cards[!present_img, ]
  if (nrow(missing_img) > 0){
    warning(paste0("Missing images for ", missing_img$id, "\n"))
  }
  
  discover_cards <- discover_cards[present_img, ]
  discover_cards$img <- paste0("www/landing/discover/", discover_cards$img)
  
  if (length(unique(discover_cards$id)) != nrow(discover_cards)) {
    stop("Discover cards do not have unique ids")
  }
  
  discover_cards$img <- sapply(discover_cards$img, base64)

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
  
  team_cards$img <- sapply(team_cards$img, base64)

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
  
  collabs$img <- sapply(collabs$img, base64)


  # Save home page information as qsm ---------------------------------------

  qs::qsavem(c_city_svg, news_cards, discover_cards,
             team_cards, contributors, collabs,
             file = paste0(data_path, "home_page.qsm")
  )
}
