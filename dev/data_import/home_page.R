## HOME PAGE TIBBLES ###########################################################

home_page <- function(data_path = "data/") {
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
  discover_cards <- tibble::tibble(
    id = c("metro", "alp", "gentrification", "access"),
    img = c(
      "www/landing/discover/pic-five-roses.jpg",
      "www/landing/discover/pic-active-living.jpg",
      "www/landing/discover/pic-biosphere.jpg",
      "www/landing/discover/pic-amenities.jpg"
    ),
    theme = c("ecology", "health", "urban", "transport"),
    en = c("The Evolution of the Montreal Metro", "Active Living Potential: The Canale Index", "Environmental racism and green gentrification", "Access to Amenities"),
    fr = c("L'évolution du métro de Montréal", "Potentiel de vie active : L'indice Canale", "Racisme environnemental et embourgeoisement vert", "Accès aux commodités")
  )


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
