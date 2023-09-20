# library(tibble)
# 
# # Translation function
# .t <- function(x) deeplr::toFrench2(x, auth_key = .deepl_key)
# translation_fun <- function(strings) {
#   z <- lapply(strings, \(x) {
#     fr <- .t(x)
#     sprintf('add_row(en = "%s", 
#           fr = "%s")', x, fr)
#   })
#   paste0(z, collapse = " |>\n") |> 
#     writeLines()
# }
# 
# # Load modules
# modules <- qs::qread("data/modules.qs")
# 
# # Change which column is subset
# strings <- modules$dataset_info
# 
# translation_fun(strings)

#nav_title
translation_pages <- 
  tibble(en = character(),
         fr = character()) |> 
  add_row(en = "Housing system", 
          fr = "Système de logement") |>
  add_row(en = "Vacancy rate", 
          fr = "Taux d'inoccupation") |>
  add_row(en = "Active living potential", 
          fr = "Potentiel de vie active") |>
  add_row(en = "Access to amenities", 
          fr = "Accès aux services et commodités") |>
  add_row(en = "Climate risk", 
          fr = "Risque climatique") |>
  add_row(en = "Natural infrastructure", 
          fr = "Infrastructures naturelles") |>
  add_row(en = "Green alleys", 
          fr = "Ruelles vertes") |>
  add_row(en = "Housing affordability", 
          fr = "Abordabilité du logement") |>
  add_row(en = "Road safety", 
          fr = "Sécurité routière") |>
  add_row(en = "Bikeway comfort and safety", 
          fr = "Confort et sécurité des voies cyclables") |>
  add_row(en = "Place explorer", 
          fr = "Explorez un lieu") |>
  add_row(en = "Montreal stories", 
          fr = "Histoires de Montréal") |> 
  add_row(en = "Tenure status", 
          fr = "Statut d'occupation") |> 
  add_row(en = "Land surface temperature",
          fr = "Température au sol") |> 
  
  #title_text_title
  add_row(en = "The housing system", 
          fr = "Le système de logement") |>
  add_row(en = "Vacancy rate", 
          fr = "Taux d'inoccupation") |>
  add_row(en = "Active Living Potential", 
          fr = "Potentiel de vie active") |>
  add_row(en = "Access to amenities", 
          fr = "Accès aux services et commodités") |>
  add_row(en = "Climate change risk", 
          fr = "Risque lié au changement climatique") |>
  add_row(en = "Natural infrastructure", 
          fr = "Infrastructures naturelles") |>
  add_row(en = "Green alleys", 
          fr = "Ruelles vertes") |>
  add_row(en = "Housing affordability", 
          fr = "Abordabilité du logement") |>
  add_row(en = "Road safety: Car crashes", 
          fr = "Sécurité routière : Collisions de voiture") |>
  add_row(en = "Place explorer", 
          fr = "Explorez un lieu") |>
  add_row(en = "Montreal stories", 
          fr = "Histoires de Montréal") |> 
  add_row(en = "Bikeway comfort and safety: the Can-BICS index",
          fr = "Confort et sécurité des pistes cyclables : l'indice Can-BICS") |> 
  
  #title_text_main
  add_row(en = "<p>Housing is at the centre of our lives. Our ability to find affordable, adequate, and healthy accommodations profoundly affects our life chances.", 
          fr = "<p>Le logement est au cœur de nos vies. Notre capacité à trouver un logement abordable, adéquat et sain affecte profondément nos chances dans la vie.") |>
  add_row(en = paste0(
    "<p>The rental vacancy rate measures the percentage of purpose-built re",
    "ntal apartments which are vacant and available for rent at a given tim",
    "e. Vacancy rates below 3% suggest a serious rental housing crisis."
  ), 
  fr = paste0(
    "Le taux d'inoccupation locatif mesure le pourcentage de logements loca",
    "tifs construits à cet effet qui sont vacants et disponibles à la locat",
    "ion à un moment donné. Les taux d'inoccupations inférieurs à 3 % indiq",
    "uent une grave crise du logement locatif."
  )
  ) |>
  add_row(en = "<p>The walkability of an area is influenced by both the built environment and socio-economic factors. The Active Living Potential index quantifies which areas provide walkable environments to their residents.", 
          fr = "<p>Le potentiel de vie active (PVA) d'une zone est influencé à la fois par l'environnement bâti et par des facteurs socio-économiques. L'indice du potentiel de vie active quantifie les zones qui offrent un environnement propice à la marche à leurs habitants.") |>
  add_row(en = paste0(
    "<p>Neighbourhood accessibility is an important contributor to quality ",
    "of life. It can be measured by how counting many amenities can be reac",
    "hed from a location for a given time and mode of transportation."
  ), 
  fr = paste0(
    "L'accessibilité au voisinage est un facteur important de la qualité de",
    " vie. Elle peut être mesurée par le nombre de services accessibles à p",
    "artir d'un lieu donné en un temps donné et par un mode de transport do",
    "nné."
  )) |>
  add_row(en = "<p>Climate change will have increasingly negative impacts on communities in Montreal, but these will vary significantly by both geographic and social factors. The distribution of five different climate risks – heat waves, flooding, heavy rain, drought, and destructive storms – is visualized here.", 
          fr = "<p>Les changements climatiques auront des impacts de plus en plus négatifs sur les communautés de Montréal, mais ceux-ci varieront considérablement en fonction des facteurs géographiques et sociaux. La distribution de cinq risques climatiques différents - vagues de chaleur, crues, pluies abondantes, sécheresse et tempêtes destructrices - est visualisée ici.") |>
  add_row(en = "<p>Natural ecosystems contribute to well-being, quality of life and public health in cities. This page quantifies the benefits provided by urban trees and wooded areas to biodiversity conservation, flood prevention, and heat-island reduction. ", 
          fr = "Les écosystèmes naturels contribuent au bien-être, à la qualité de vie et à la santé publique dans les villes. Cette page quantifie les avantages apportés par les arbres urbains et les zones boisées à la conservation de la biodiversité, à la prévention des inondations et à la réduction des îlots de chaleur.") |>
  add_row(en = "<p>Green alleys are spaces that have been transformed by residents for their own activities. When adequately designed, these public spaces can help reduce heat island effects, noise, and air pollution.", 
          fr = "Les ruelles vertes sont des espaces transformés par les habitants pour leurs propres activités. Lorsqu'elles sont bien conçus, ces espaces publics peuvent contribuer à réduire l'effet d'îlot de chaleur, le bruit et la pollution de l'air.") |>
  add_row(en = paste0(
    "<p><img src='centraide_logo/centraide_logo_en.png' style='width:60%;ma",
    "rgin-left:50px;'><p>Access to affordable housing is a fundamental huma",
    "n right, yet it remains a pressing challenge for many communities. Hou",
    "sing is often defined as affordable when a household spends less than ",
    "30% of its income on shelter, but the reality for each household is mu",
    "ch more complex."
  ), 
  fr = "<p><img src='centraide_logo/centraide_logo_fr.png' style='width:60%;margin-left:50px;'>L'accès à un logement abordable est un droit de l'homme fondamental, mais il reste un défi pressant pour de nombreuses communautés. Le logement est souvent défini comme abordable lorsqu'un ménage dépense moins de 30 % de ses revenus pour se loger, mais la réalité de chaque ménage est beaucoup plus complexe.") |>
  add_row(en = paste0("<p>Road safety is an important consideration for wellbeing and safety ",
                      "in cities. This page displays all road collisions in the City of Montr",
                      "eal."
  ), 
  fr = paste0(
    "La sécurité routière est un élément important pour le bien-être et la ",
    "sécurité dans les villes. Cette page affiche toutes les collisions rou",
    "tières dans la ville de Montréal."
  )
  ) |>
  add_row(en = paste0(
    "<p>Road safety is an important consideration for wellbeing and safety ",
    "in cities. This page displays all road collisions in the City of Montr",
    "eal."
  ), 
  fr = "<p>La sécurité routière est une considération importante pour le bien-être et la sécurité dans les villes. Ce module fournit une vue d'ensemble et une analyse des collisions routières dans la ville de Montréal, de 2012 à aujourd'hui.") |>
  add_row(en = "Select a location by entering a postal code or clicking on the map to see how it compares to the rest of the region across a variety of sustainability indicators.", 
          fr = "Sélectionnez un lieu en entrant un code postal ou en cliquant sur la carte pour voir comment il se situe par rapport au reste de la région en fonction d'une série d'indicateurs de durabilité.") |>
  add_row(en = "Explore narrative case studies about specific urban sustainability and planning issues in the Montreal region.", 
          fr = "Explorer des études de cas narratives sur des questions spécifiques de durabilité urbaine et de planification dans la région de Montréal.") |> 
  add_row(en = paste0(
    "<p><img src='centraide_logo/centraide_logo_en.png' style='width:60%;ma",
    "rgin-left:50px;'><p>Tenure status measures whethera household owns or ",
    "rents its home. Housing needs can vary dramatically by tenure status."
  ), fr = "<p><img src='centraide_logo/centraide_logo_fr.png' style='width:60%;margin-left:50px;'>Le statut d'occupation mesure si un ménage est propriétaire ou locataire de son logement. Les besoins en matière de logement peuvent varier considérablement en fonction du statut d'occupation.") |> 
  add_row(en = "Land surface temperature measures the maximum mean warm-season temperature at a specific location. It is a crucial indicator of urban heat islands and ecological balance within a region.",
          fr = "La température au sol mesure la température moyenne maximale de la saison chaude à un endroit précis. C'est un indicateur essentiel des îlots de chaleur urbains et de l'équilibre écologique au sein d'une région.") |> 
  #title_text_extra 
  add_row(en = "<p>The datasets visualized on this page come from the Canadian Census from 1996 to the present. There are a few efforts in place to better the housing landscape from the federal and municipal governments. In Canada, the National Housing Strategy aims to address housing needs and houselessness through modernization, new construction, and innovation and research. Within the City of Montreal, important housing initiatives include the Diverse Metropolis by-law and the 12,000-housing unit strategy. For more information on these initiatives visit:<ul><li><a href='https://www.cmhc-schl.gc.ca/en/nhs/', target = '_blank'>CMHC. (n.d.). National Housing Strategy</a><li><a href='https://montreal.ca/articles/metropole-mixte-les-grandes-lignes-du-reglement-7816', target = '_blank'>Ville de Montréal. (4 octobre 2021). Métropole Mixte: Les grandes lignes du règlement.</a>", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent du recensement canadien de 1996 à aujourd'hui. Les gouvernements fédéral et municipaux ont déployé quelques efforts pour améliorer le paysage du logement. Au Canada, la stratégie nationale du logement vise à répondre aux besoins en matière de logement et à lutter contre le sans-abrisme par la modernisation, la construction de nouveaux logements, l'innovation et la recherche. Au sein de la ville de Montréal, les initiatives importantes en matière de logement comprennent le règlement Métropole mixte et la stratégie de développment de 12 000 logements. Pour plus d'informations sur ces initiatives, visitez :<ul><li><a href='https://www.cmhc-schl.gc.ca/en/nhs/', target = '_blank'>CMHC. (s.d.). Stratégie nationale sur le logement</a><li><a href='https://montreal.ca/articles/metropole-mixte-les-grandes-lignes-du-reglement-7816', target = '_blank'>Ville de Montréal. (4 octobre 2021). Métropole Mixte : Les grandes lignes du règlement.</a>") |>
  add_row(en = "<p>The datasets visualized on this page come from Curbcut using data from the Canadian Censuses and DMTI. Our index considers street connectivity, building density, and points of interest. Active Living Potential is then calculated based on dissemination areas accessible within a 15-minute walk. The work on this page was highly influenced by the <a href = 'http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf', target = '_blank'>CanALE index</a> developed by Prof. Nancy Ross and her team.", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent de Curbcut, à partir des données des recensements canadiens et de DMTI. Notre indice tient compte de la connectivité des rues, de la densité des bâtiments et des points d'intérêt. Le potentiel de vie active est ensuite calculé en fonction des aires de diffusion accessibles en moins de 15 minutes de marche. Le travail sur cette page a été fortement influencé par le <a href = 'http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf', target = '_blank'>CanALE index</a> développé par la professeur Nancy Ross et son équipe.") |>
  add_row(en = "<p>Curbcut has calculated travel times for walking, cycling, and driving using the Open Source Routing Machine (OSRM) and the OpenStreetMap (OSM) street network. For transit travel times, Curbcut has employed GTFS feeds and a multimodal approach, incorporating walking times derived from OSRM and the OSM street network. The amenities data has been sourced from a combination of DMTI Spatial and OpenStreetMap.", 
          fr = "<p>Curbcut a calculé les temps de parcours pour la marche, le vélo et la voiture en utilisant l'Open Source Routing Machine (OSRM) et le réseau de rues OpenStreetMap (OSM). Pour les temps de trajet en transport en commun, Curbcut a utilisé les flux GTFS et une approche multimodale, en incorporant les temps de marche dérivés de l'OSRM et du réseau de rues OSM. Les données sur les aménagements proviennent d'une combinaison de DMTI Spatial et d'OpenStreetMap.") |>
  add_row(en = "<p>The datasets visualized on this page come from Habitat. Note that the natural infrastructure included in the study that generated this data only covers approximately 25% of the Montreal region. For more information on the methods and data used for this page, visit the publication <a href = 'https://fr.davidsuzuki.org/publication-scientifique/le-role-des-infrastructures-naturelles-dans-la-prevention-des-inondations-dans-la-communaute-metropolitaine-de-montreal/ ' target = '_blank'>“Le rôle des infrastructures naturelles dans la prévention des inondations dans la Communauté métropolitaine de Montréal”</a>.", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent d'Habitat. Il est à noter que les infrastructures naturelles incluses dans l'étude qui a généré ces données ne couvrent qu'environ 25 % de la région de Montréal. Pour plus d'information sur les méthodes et les données utilisées pour cette page, visitez la publication <a href = 'https://fr.davidsuzuki.org/publication-scientifique/le-role-des-infrastructures-naturelles-dans-la-prevention-des-inondations-dans-la-communaute-metropolitaine-de-montreal/ ' target = '_blank'>Le rôle des infrastructures naturelles dans la prévention des inondations dans la Communauté métropolitaine de Montréal</a>.") |>
  add_row(en = "<p>The datasets visualized on this page come from the 2016 and 2021 Canadian Censuses.", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent des recensements canadiens de 2016 et de 2021.") |>
  add_row(en = "<p>Data is collected by the Service de Police de la Ville de Montréal (SPVM) and compiled by the Société d'Assurance Automobile du Québec (SAAQ), and contains information related to every road collision, including the date, location and type of parties involved (i.e. cars, bicycles or pedestrians) and injury severity. <p>For more information on road collisions and a temporal analysis of the data, please consult the <i>Road safety analysis</i> below.</p><p>References:</p><ul><li><a href = https://www.pietons.quebec/sites/default/files/documents/pietonsqc_vf_fiche_decouvrirapprochevisionzerosecuriteroutiere.pdf> Piétons Québec. (2021). Découvrir l'approche vision zéro en sécurité routière. Piétons Québec. Online:</a><li><a href='https://donnees.montreal.ca/ville-de-montreal/collisions-routieres'>Ville de Montréal. (2021). Collisions routières. Données Ouvertes Montréal.</a></ul>", 
          fr = "<p>Les données sont recueillies par le Service de police de la Ville de Montréal (SPVM) et compilées par la Société d'assurance automobile du Québec (SAAQ). Elles contiennent des informations relatives à chaque collision routière, notamment la date, le lieu et le type de personnes impliquées (voitures, vélos ou piétons) ainsi que la gravité des blessures. <Pour plus d'information sur les collisions routières et une analyse temporelle des données, veuillez consulter l'<i>Analyse de la sécurité routière</i> ci-dessous.</p><p>Références:</p><ul><li><a href = https://www.pietons.quebec/sites/default/files/documents/pietonsqc_vf_fiche_decouvrirapprochevisionzerosecuriteroutiere.pdf> Piétons Québec. (2021). Découvrir l'approche vision zéro en sécurité routière. Piétons Québec. En ligne:</a><li><a href='https://donnees.montreal.ca/ville-de-montreal/collisions-routieres'>Ville de Montréal. (2021). Collisions routières. Données Ouvertes Montréal.</a></ul>") |>
  add_row(en = "<p>The data in the Place Explorer is taken from other Curbcut pages with the exception of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>Air pollution</a>.", 
          fr = "<p>Les données de l'explorateur de lieux proviennent d'autres pages de Curbcut, à l'exception de la <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>Pollution de l'air</a>.") |>
  add_row(en = "<p>These narrative case studies are written by the Curbcut team and its contributors.", 
          fr = "<p>Ces études de cas narratives sont rédigées par l'équipe de Curbcut et ses contributeurs.") |> 
  #dataset_info
  add_row(en = "<p>This page presents <a href = 'https://www.statcan.gc.ca/en/census/census-engagement/about'>housing data from the 1996 to the latest, Canadian Censuses</a></p>", 
          fr = "<p>Cette page présente <a href = 'https://www.statcan.gc.ca/en/census/census-engagement/about'>les données d'hébergement des recensements canadiens de 1996 à aujourd'hui.</a>") |>
  add_row(en = "<p>This module presents <a href = 'https://www.statcan.gc.ca/en/census/census-engagement/about'>housing data from the 1996 to the latest, Canadian Censuses</a></p>", 
          fr = "<p>Cette page présente <a href = 'https://www.statcan.gc.ca/en/census/census-engagement/about'>les données d'hébergement des recensements canadiens de 1996 à aujourd'hui.</a>") |>
  add_row(en = "<p>The datasets visualized on this page come from the CMHC and the 2021 Canadian Census.", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent de la SCHL et du recensement canadien de 2021.") |>
  add_row(en = "<p>The data visualized on this page come from Curbcut. Active Living Potential (ALP) is an index created using datasets from DMTI, Statistics Canada road network files and the Canadian Census. The index considers three variables—street connectivity, building density, and points of interest—for which high values collectively describe areas that strongly support active living. The percentile of each variable is calculated at the dissemination area scale, based on dissemination areas accessible within a 15-minute walk from a dissemination area centroid, and the sum of these percentiles is the ALP index value. The dataset is calculated from 2001 through 2021 in five-year intervals (corresponding to Census years). Our ALP index was highly influenced by the <a href = 'http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf', target = '_blank'>CanALE index developed by Ross et al. (2018)</a>. Our index differs by calculating a buffer using a 15-minute walk on the street network using our internal travel time matrix dataset instead of a 1km buffer around the centroid of dissemination areas. Our index also differs by using a sum of percentiles rather than a sum of z-scores. This method reduces the influence of extreme outliers, especially in the case of points of interest which have a very large variance. Thus, this percentile approach offers a balanced and nuanced understanding of an area's walkability.", 
          fr = "<p>Les données visualisées sur cette page proviennent de Curbcut. Le potentiel de vie active (PVA) est un indice créé à partir d'ensembles de données provenant de DMTI, de fichiers du réseau routier de Statistique Canada et du recensement canadien. L'indice prend en compte trois variables - la connectivité des rues, la densité des bâtiments et les points d'intérêt - pour lesquelles des valeurs élevées décrivent collectivement des zones qui favorisent fortement la vie active. Le centile de chaque variable est calculé à l'échelle de l'aire de diffusion, sur la base des aires de diffusion accessibles à moins de 15 minutes de marche du centroïde d'une aire de diffusion, et la somme de ces centiles constitue la valeur de l'indice ALP. L'ensemble des données est calculé de 2001 à 2021 par intervalles de cinq ans (correspondant aux années de recensement). Notre indice PVA a été fortement influencé par l'indice <a href = 'http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf', target = '_blank'>CanALE développé par Ross et al. (2018)</a>. Notre indice diffère en calculant un tampon à l'aide d'une marche de 15 minutes sur le réseau routier/piétonnier en utilisant notre ensemble de données interne, une matrice de temps de déplacement, au lieu d'un tampon de 1 km autour du centroïde des aires de diffusion. Notre indice diffère également en utilisant une somme de percentiles plutôt qu'une somme de cotes Z. Cette méthode réduit l'influence des valeurs aberrantes extrêmes, en particulier dans le cas des points d'intérêt qui ont une très grande variance. Ainsi, cette approche par percentile offre une compréhension équilibrée et nuancée de la marchabilité d'une zone.") |>
  add_row(en = "<p>Curbcut has developed a comprehensive methodology for calculating travel times and determining accessibility to various amenities. Travel times for walking, cycling, and driving have been calculated using the Open Source Routing Machine (OSRM) and the OpenStreetMap (OSM) street network. Transit travel times are calculated using GTFS feeds and a multimodal approach that incorporates walking times from OSRM and the OSM street network.<p>To analyze the accessibility to amenities, Curbcut utilizes a two-step process. First, the travel time distances are calculated using a custom function which computes the shortest travel times between Dissemination Area (DA) centroids and their closest neighbors within specified distances. Next, the number of accessible amenities within each DA is determined by joining amenity points with the DA boundaries and counting the number of intersections.<p>The amenities data is sourced from a combination of DMTI Spatial and OpenStreetMap, ensuring an accurate representation of various types of amenities in the area. This methodology allows for a detailed analysis of travel times and accessibility to amenities, providing valuable insights for urban planning and development purposes.", 
          fr = "<p>Curbcut a développé une méthodologie complète pour calculer les temps de trajet et déterminer l'accessibilité à diverses services et commodités. Les temps de parcours pour la marche, le vélo et la voiture ont été calculés à l'aide de l'Open Source Routing Machine (OSRM) et du réseau de rues OpenStreetMap (OSM). Les temps de trajet en transport en commun sont calculés à l'aide des flux GTFS et d'une approche multimodale qui incorpore les temps de marche de l'OSRM et du réseau de rues OSM.<p>Pour analyser l'accessibilité aux commodités, Curbcut utilise un processus en deux étapes. Tout d'abord, les distances de temps de trajet sont calculées à l'aide d'une fonction personnalisée qui calcule les temps de trajet les plus courts entre les centroïdes de l'aire de diffusion (AD) et leurs voisins les plus proches dans des distances spécifiées. Ensuite, le nombre d'équipements accessibles dans chaque aire de diffusion est déterminé en joignant les points d'équipements aux limites de l'aire de diffusion et en comptant le nombre d'intersections.<p>Les données sur les services et commodités proviennent d'une combinaison de DMTI Spatial et d'OpenStreetMap, ce qui garantit une représentation précise des différents types d'équipements dans la région. Cette méthodologie permet une analyse détaillée des temps de parcours et de l'accessibilité aux équipements, fournissant des informations précieuses pour la planification urbaine et le développement.") |>
  add_row(en = "<p><a href = 'https://donnees.montreal.ca/ville-de-montreal/vulnerabilite-changements-climatiques'>The data presented on this page are cartographic representations of the vulnerability analysis</a> developed as part of the Climate change adaptation plan for the agglomeration of Montréal 2015-2020 for the following climate hazards: heavy rainfall, heat waves, destructive storms, droughts and floods.</p>", 
          fr = "<p><a href = 'https://donnees.montreal.ca/ville-de-montreal/vulnerabilite-changements-climatiques'>Les données présentées sur cette page sont des représentations cartographiques de l'analyse de vulnérabilité</a> développée dans le cadre du Plan d'adaptation aux changements climatiques de l'agglomération de Montréal 2015-2020 pour les aléas climatiques suivants : pluies abondantes, vagues de chaleur, tempêtes destructrices, sécheresses et inondations.</p>") |>
  add_row(en = "<p>Data made available by the firm Habitat. For more information on the methods and data used for this page, see <a href = 'https://fr.davidsuzuki.org/publication-scientifique/le-role-des-infrastructures-natrelles-dans-la-prevention-des-inondations-dans-la-communaute-metropolitaine-de-montreal/'>Maure et al., 2018, Le rôle des infrastructures naturelles dans la prévention des inondations dans la Communauté métropolitaine de Montréal, Fondation David Suzuki.</a></p>", 
          fr = "<p>Données mises à disposition par l'entreprise Habitat. Pour plus d'informations sur les méthodes et les données utilisées pour cette page, voir <a href = 'https://fr.davidsuzuki.org/publication-scientifique/le-role-des-infrastructures-natrelles-dans-la-prevention-des-inondations-dans-la-communaute-metropolitaine-de-montreal/'>Maure et al., 2018, Le rôle des infrastructures naturelles dans la prévention des inondations dans la Communauté métropolitaine de Montréal, Fondation David Suzuki.</a></p>") |>
  add_row(en = "<p>The census data (2016-2021) on this page comes from custom tabulations from Statistics Canada ordered by Centraide of Greater Montreal</p>", 
          fr = "<p>Les données du recensement (2016-2021) présentées sur cette page proviennent de tabulations personnalisées de Statistique Canada commandées par Centraide du Grand Montréal.") |> 
  add_row(en = "<p>This dataset, provided by the Canadian Urban Environmental Health Research Consortium, includes annual estimates of LST developed using a public algorithm in Google Earth Engine. The data, derived from LandSat 8 imagery, represents a 3 years annual maximum mean warm-season land surface temperature.</p>",
          fr = "<p>Ce jeu de données, fourni par le Consortium canadien de recherche en santé environnementale urbaine, comprend des estimations annuelles de la température de surface terrestre élaborées à l'aide d'un algorithme public dans Google Earth Engine. Les données, dérivées de l'imagerie LandSat 8, représentent une moyenne annuelle maximale de 3 ans de la température de surface des terres en saison chaude.</p>") |> 
  add_row(en = "<p>We built an extensive dataset of green alleys across Montreal, which were categorized as either green or community alleys. Our data was carefully collected and validated, using various resources both public and private. Here are our key data sources:</p><ul><li><a href=’https://donnees.montreal.ca/dataset/ruelles-vertes’ target=’_blank’>Official Montreal Open Data Portal - Green Alleys Dataset</a></li><li><a href=’https://www.google.com/maps/d/viewer?mid=143hjP-d1kJ9dlifQF_2jtys85B4&ll=45.52200058770156%2C-73.47754611620758&z=11’ target=’_blank’>Google Maps Green Alleys - 2022 Version</a></li><li>Specific Maps Sent by the borough of Mercier-Hochelaga-Maisonneuve (These are not publicly accessible) </li><li><a href=’https://docs.google.com/spreadsheets/d/1gbNQnEErVOQdfN95Fg0uPHJitKC-JAEZ/edit#gid=486678456’ target=’_blank’>Montreal-Nord Green Alleys List</a></li><li> Montréal-Nord Green Alleys Map sent by the borough</li><li> NDG Green Alleys Map sent by the borough</a></li></ul><p>In addition to the above-mentioned sources, our team also incorporated green alleys that we discovered during our site visits. This combination of digital data and on-ground exploration allows us to create a comprehensive and authentic catalogue of Montreal's green alleys. </p>",
          fr = "Nous avons construit un vaste ensemble de données sur les ruelles vertes à travers Montréal, qui ont été catégorisées comme ruelles vertes ou ruelles communautaires. Nos données ont été soigneusement collectées et validées en utilisant diverses ressources, à la fois publiques et privées. Voici nos principales sources de données :</p><ul><li><a href='https://donnees.montreal.ca/dataset/ruelles-vertes' target='_blank'>Portail officiel de données ouvertes de Montréal - Ensemble de données sur les ruelles vertes</a></li><li><a href='https://www.google.com/maps/d/viewer?mid=143hjP-d1kJ9dlifQF_2jtys85B4&ll=45.52200058770156%2C-73.47754611620758&z=11' target='_blank'>Google Maps des ruelles vertes - Version 2022</a></li><li>Cartes spécifiques envoyées par l'arrondissement de Mercier-Hochelaga-Maisonneuve (Ces données ne sont pas accessibles au public)</li><li><a href='https://docs.google.com/spreadsheets/d/1gbNQnEErVOQdfN95Fg0uPHJitKC-JAEZ/edit#gid=486678456' target='_blank'>Liste des ruelles vertes de Montréal-Nord</a></li><li>Carte des ruelles vertes de Montréal-Nord envoyée par l'arrondissement</li><li>Carte des ruelles vertes de NDG envoyée par l'arrondissement</li></ul><p>En plus des sources mentionnées ci-dessus, notre équipe a également intégré des ruelles vertes que nous avons découvertes lors de nos visites sur site. Cette combinaison de données numériques et d'exploration sur le terrain nous permet de créer un catalogue complet et authentique des ruelles vertes de Montréal.</p>") |> 
  
  
  # CANBICS
  add_row(en = paste0(
    "Can-BICS, or Canadian Bikeway Comfort and Safety, is a classification ",
    "system for rating cycling infrastructure across Canada on safety and u",
    "ser comfort."
  ),
  fr = paste0(
    "Can-BICS, ou Canadian Bikeway Comfort and Safety, est un système de cl",
    "assification permettant d'évaluer les infrastructures cyclables au Can",
    "ada en fonction de la sécurité et du confort de l'utilisateur."
  )) |> 
  add_row(en = "<p><a target = '_blank' href = 'https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html'>The Canadian Bikeway Comfort and Safety (Can-BICS) Classification System</a> dataset is a geographic-based set of measures characterizing the cycling infrastructure of Canadian communities. The data is initially provided at the dissemination area level.</p>",
          fr = "<p><a target = '_blank' href = 'https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html'>Le système de classification du confort et de la sécurité des pistes cyclables au Canada (Can-BICS)</a> est un ensemble de mesures géographiques caractérisant l'infrastructure cyclable des collectivités canadiennes. Les données sont initialement fournies au niveau de l'aire de diffusion.") |> 
  
  # NDVI
  add_row(en = paste0(
    "<p>The Normalized Difference Vegetation Index (NDVI) measures the aver",
    "age amount of vegetation present in an area during the growing season.",
    " It is used for environmental conservation, urban planning, and climat",
    "e change mitigation."
  ), 
  fr = paste0(
    "L'indice de végétation par différence normalisée (NDVI) mesure la quan",
    "tité moyenne de végétation présente dans une zone pendant la saison de",
    " culture. Il est utilisé pour la conservation de l'environnement, la p",
    "lanification urbaine et l'atténuation du changement climatique."
  )) |> 
  add_row(en = paste0(
    "<p>The NDVI data on this page is derived from the HLSS30.v2.0 and HLSL", 
    "30.v2.0 satellites, spanning from 2013 to the present. The process inc", 
    "ludes the following detailed steps:</p><ul><li><strong>Data Retrieval:", 
    "</strong> Specific NDVI bands are extracted based on the satellite col", 
    "lection. These bands are used to analyze the vegetation intensity and ", 
    "are organized into a data frame.</li><li><strong>Raster Stacking:</str", 
    "ong> Three bands (Red, Near-infrared (NIR), and Fmask) are extracted, cropped, and mas", 
    "ked according to the area of interest. These bands are essential for a", 
    "nalyzing vegetation patterns and filtering out cloud contamination.</l", 
    "i><li><strong>NDVI Calculation:</strong> NDVI is computed using the fo", 
    "rmula (NIR-Red)/(NIR+Red), quantifying the vegetation's health. This i", 
    "ndex measures the difference between near-infrared (which vegetation s", 
    "trongly reflects) and red light (which vegetation absorbs).</li><li><s", 
    "trong>Quality Filtering with Fmask:</strong> The Fmask (Function of Ma", 
    "sk) band is utilized to filter out poor quality pixels. Fmask is an al", 
    "gorithm that identifies and masks unwanted features like clouds, cloud", 
    " shadows, water, or snow/ice. In the HLS data, values of 0 and 64 in t", 
    "he Fmask layer indicate clean and useful pixels, ensuring that the res", 
    "ulting NDVI is free from these artifacts. This filtering is vital for ", 
    "obtaining a true state of the vegetation and underlying surface.</li><", 
    "li><strong>Resampling:</strong> All NDVI rasters are resampled to a sp", 
    "ecific extent to align perfectly, allowing for accurate aggregation.</", 
    "li></ul><p>This process, with its meticulous handling of NDVI bands an", 
    "d quality filtering using Fmask, provides a scientifically robust view", 
    " of vegetation trends, essential for urban sustainability studies and ", 
    "environmental justice analyses.</p>"), 
    fr = paste0(
      "<p>Les données NDVI présentées sur cette page sont dérivées des s", 
      "atellites HLSS30.v2.0 et HLSL30.v2.0, couvrant une période allant de 2", 
      "013 à aujourd'hui. Le processus comprend les étapes détaillées suivant", 
      "es:</p><ul><li><strong>Extraction de données:</strong> Des bandes NDVI spécifi", 
      "ques sont extraites sur la base de la collecte satellitaire. Ces bande", 
      "s sont utilisées pour analyser l'intensité de la végétation et sont or", 
      "ganisées.</li><li><strong>Empilement des bandes", 
      ":</strong> Trois bandes (Rouge, proche infrarouge (NIR), et Fmask) sont extraites, recadr", 
      "ées et masquées en fonction de la zone d'intérêt. Ces bandes sont esse", 
      "ntielles pour analyser les modèles de végétation et filtrer la contami", 
      "nation par les nuages.</li><li><strong>Calcul du NDVI:</strong> Le NDV", 
      "I est calculé à l'aide de la formule (NIR-Rouge)/(NIR+Rouge), qui quan", 
      "tifie l'état de santé de la végétation. Cet indice mesure la différenc", 
      "e entre le proche infrarouge (que la végétation reflète fortement) et ", 
      "la lumière rouge (que la végétation absorbe).</li><li><strong>Filtrage", 
      " de qualité avec Fmask:</strong> La bande Fmask (Function of Mask) est", 
      " utilisée pour filtrer les pixels de mauvaise qualité. Fmask est un al", 
      "gorithme qui identifie et masque les caractéristiques indésirables tel", 
      "les que les nuages, les ombres des nuages, l'eau ou la neige/glace. Da", 
      "ns les données HLS, les valeurs de 0 et 64 dans la couche Fmask indiqu", 
      "ent des pixels propres et utiles, ce qui garantit que le NDVI résultan", 
      "t est exempt de ces artefacts. Ce filtrage est essentiel pour obtenir ", 
      "un état réel de la végétation et de la surface sous-jacente.</li><li><", 
      "strong>Réséchantillonnage:</strong> Tous les images matricielles NDVI sont rééchan", 
      "tillonnés dans une certaine mesure pour s'aligner parfaitement, ce qui", 
      " permet une agrégation précise.</li></ul><p>Ce processus, avec son tra", 
      "itement méticuleux des bandes NDVI et son filtrage de qualité à l'aide", 
      " de Fmask, fournit une vue scientifiquement robuste des tendances de l", 
      "a végétation, essentielle pour les études de durabilité urbaine et les", 
      " analyses de la justice environnementale.</p>")) |> 
  
  # Main dropdown titles
  add_row(en = "Vacancy rate distribution",
          fr = "Distribution des taux d'inoccupation") |> 
  add_row(en = "Climate vulnerability indicator",
          fr = "Vulnérabilité climatique") |> 
  add_row(en = "Data representation",
          fr = "Représentation de données") |> 
  add_row(en = "Car crash",
          fr = "Collision de voiture") |> 
  add_row(en = "Crash type",
          fr = "Type de collision") |> 
  
  
  # ID
  add_row(en = "housing", 
          fr = "logement") |> 
  add_row(en = "vacancy", 
          fr = "inoccupation") |> 
  add_row(en = "alp", 
          fr = "pva") |> 
  add_row(en = "ndvi", 
          fr = "ndvi") |> 
  add_row(en = "lst", 
          fr = "lst") |> 
  add_row(en = "canbics", 
          fr = "canbics") |> 
  add_row(en = "access", 
          fr = "acces") |> 
  add_row(en = "climate_risk", 
          fr = "risque_climatique") |> 
  add_row(en = "natural_inf", 
          fr = "inf_naturelle") |> 
  add_row(en = "alley", 
          fr = "ruelle") |> 
  add_row(en = "safety", 
          fr = "securite") |> 
  add_row(en = "tenure", 
          fr = "occupation") |> 
  add_row(en = "afford", 
          fr = "affordabilite") |> 
  add_row(en = "place_explorer", 
          fr = "explorateur_lieu") |> 
  add_row(en = "stories", 
          fr = "histoires") |> 
  
  
  
  
  # New title_text_extra
  add_row(en = paste0(
    "<p>The vacancy rate is the most important indicator of rental housing ",
    "availability. A higher rate means more available housing, and a lower ",
    "rate means the opposite. The datasets visualized on this page come fro",
    "m the CMHC and the 2021 Canadian Census."
  ),
  fr = "Le taux d'inoccupation est l'indicateur le plus important de la disponibilité des logements locatifs. Un taux plus élevé signifie qu'il y a plus de logements disponibles, et un taux plus bas signifie le contraire. Les ensembles de données visualisés sur cette page proviennent de la SCHL et du recensement canadien de 2021.") |> 
  add_row(en = paste0(
    "<p>Can-BICS rates bikeways into three tiers: high-, medium-, and low-c",
    "omfort. The datasets visualized on this page come from CANUE and the 2",
    "021 Canadian Census. Can-BICS was developed by Meghan Winters and her ",
    "team. Understanding the spatialization of cycling infrastructure as cl",
    "assified by Can-BICS can help to highlight the availability and infras",
    "tructure types across a region and support efforts in improving bikewa",
    "ys. For more information about Can-BICS visit: <a target = '_blank' hr",
    "ef='https://www.canada.ca/en/public-health/services/reports-publicatio",
    "ns/health-promotion-chronic-disease-prevention-canada-research-policy-",
    "practice/vol-40-no-9-2020/canbics-classification-system-naming-convent",
    "ion-cycling-infrastructure.html'>At-a-glance – The Canadian Bikeway Co",
    "mfort and Safety (Can-BICS) Classification System: a common naming con",
    "vention for cycling infrastructure</a>"
  ),
  fr = paste0(
    "Can-BICS classe les pistes cyclables en trois catégories : confort éle",
    "vé, moyen et faible. Les ensembles de données visualisés sur cette pag",
    "e proviennent de CANUE et du recensement canadien de 2021. Can-BICS a ",
    "été développé par Meghan Winters et son équipe. Comprendre la spatiali",
    "sation des infrastructures cyclables telles que classées par Can-BICS ",
    "peut aider à mettre en évidence la disponibilité et les types d'infras",
    "tructures dans une région et soutenir les efforts d'amélioration des p",
    "istes cyclables.Pour plus d'informations sur Can-BICS, visitez :<a tar",
    "get = '_blank' href=' https://www.canada.ca/fr/sante-publique/services",
    "/rapports-publications/promotion-sante-prevention-maladies-chroniques-",
    "canada-recherche-politiques-pratiques/vol-40-no-9-2020/systeme-classif",
    "ication-canbics-convention-appellation-amenagements-cyclables.html> Ap",
    "erçu – Système de classification du confort et de la sécurité des voie",
    "s cyclables canadiennes (Can-BICS) : convention d'appellation commune ",
    "des aménagements cyclables</a>"
  )) |> 
  add_row(en = paste0(
    "<p>This data represents the highest mean warm-season temperature recor",
    "ded at a location over a three-year span, helping to minimize the impa",
    "ct of missing data or cloud cover. LST is instrumental in identifying ",
    "areas that are hotter during the day and more likely to radiate excess",
    " heat at night, contributing to urban heat phenomena. Understanding LS",
    "T is essential for urban planning, health assessments, and environment",
    "al protection. To learn more about how LST is calculated, <a href='htt",
    "ps://www.canuedata.ca/tmp/CANUE_METADATA_WTLST_AVA_YY.pdf' target='_bl",
    "ank'>click here</a>.</p>"
  ),
  fr = paste0(
    "Ces données représentent la température moyenne la plus élevée de la s",
    "aison chaude enregistrée à un endroit donné sur une période de trois a",
    "ns, ce qui permet de minimiser l'impact des données manquantes ou de l",
    "a couverture nuageuse. La LST permet d'identifier les zones les plus c",
    "haudes pendant la journée et les plus susceptibles d'émettre un excès ",
    "de chaleur pendant la nuit, contribuant ainsi aux phénomènes de chaleu",
    "r urbaine. La compréhension de la température ambiante est essentielle",
    " pour la planification urbaine, les évaluations sanitaires et la prote",
    "ction de l'environnement. Pour en savoir plus sur le calcul, <a href='",
    "https://www.canuedata.ca/tmp/CANUE_METADATA_WTLST_AVA_YY.pdf' target='",
    "_blank'>cliquez ici</a>."
  )) |> 
  add_row(en = paste0(
    "<p>NDVI plays a significant role in various applications, including an",
    "alyzing urban greenness, monitoring agricultural growth, and assessing",
    " wildfire risks. Calculated from Harmonized Landsat Sentinel-2 (HLS) d",
    "ata, NDVI represents average vegetation during the growing season (May",
    " 1st through August 31st)."
  ),
  fr = paste0(
    "Le NDVI joue un rôle important dans diverses applications, notamment l",
    "'analyse de la verdure en milieu urbain, le suivi de la croissance agr",
    "icole et l'évaluation des risques d'incendie de forêt. Calculé à parti",
    "r des données harmonisées de Landsat Sentinel-2 (HLS), le NDVI représe",
    "nte la végétation moyenne pendant la saison de croissance (du 1er mai ",
    "au 31 août)."
  )) |> 
  add_row(en = paste0(
    "<p>The datasets visualized on this page are publicly available through",
    " the <a href = 'https://donnees.montreal.ca/dataset/vulnerabilite-chan",
    "gements-climatiques' target = '_blank'>Montreal Open Data Portal</a>. ",
    "These were developed as part of the City of Montreal's efforts to exam",
    "ine potential climate risks for the Montreal region in the <a href = '",
    "https://ville.montreal.qc.ca/pls/portal/docs/page/enviro_fr/media/docu",
    "ments/paccam_2015-2020_lesconstats.pdf' target = '_blank'>2015-2020 Ur",
    "ban Agglomeration Climate Change Adaptation Plan</a>."
  ),
  fr = paste0(
    "Les ensembles de données visualisés sur cette page sont accessibles au",
    " public via le <a href = 'https://donnees.montreal.ca/dataset/vulnerab",
    "ilite-changements-climatiques' target = '_blank'>portail de données ou",
    "vertes de la Ville de Montréal</a>. Ils ont été élaborés dans le cadre",
    " des efforts déployés par la Ville de Montréal pour examiner les risqu",
    "es climatiques potentiels pour la région de Montréal dans le<a href = ",
    "'Plan d'adaptation aux changements climatiques de l'agglomération de M",
    "ontréal 2015-2020 (montreal.qc.ca)' target = '_blank'> Plan d'adaptati",
    "on aux changements climatiques de l'agglomération de Montréal 2015-202",
    "0</a>."
  )) |> 
  add_row(en = "<p>Montrea's green alleys have been classified into four types: green alleys, community-oriented alleys, mixed alleys, and unmaintained alleys. The datasets visualized on this page come from the City of Montreal Open Data Portal and Curbcut. To learn more about the Green Alley Program in Montreal, visit <a href = ‘https://montreal.ca/en/topics/green-alleyways’ target = ‘_blank’>the City’s green alleyways page</a>. ",
  fr = paste0(
    "Les ruelles vertes de Montréal ont été classées en quatre types : les ",
    "ruelles vertes, les ruelles communautaires, les ruelles mixtes et les ",
    "ruelles non entretenues. Les jeux de données visualisés sur cette page",
    " proviennent du Portail de données ouvertes de la Ville de Montréal et",
    " de Curbcut. Pour en savoir plus sur le programme des ruelles vertes à",
    " Montréal, visitez <a href = 'https://montreal.ca/sujets/ruelles-verte",
    "s' target = '_blank'>la page de la Ville au sujet des ruelles vertes</",
    "a>."
  )) |> 
  add_row(en = "<p>The datasets visualized on this page come from Habitat. Note that the natural infrastructure included in the study that generated this data only covers approximately 25% of the Montreal region. For more information on the methods and data used for this page, visit the publication <a href = ‘https://fr.davidsuzuki.org/publication-scientifique/le-role-des-infrastructures-naturelles-dans-la-prevention-des-inondations-dans-la-communaute-metropolitaine-de-montreal/ ‘ target = ‘_blank’>“Le rôle des infrastructures naturelles dans la prévention des inondations dans la Communauté métropolitaine de Montréal”</a>.",
  fr = paste0(
    "Les ensembles de données visualisés sur cette page proviennent de H",
    "abitat. Notez que les infrastructures naturelles incluses dans l'étude",
    " ayant généré ces données couvrent seulement environ 25% de la région ",
    "de Montréal. Pour plus d'informations sur les méthodes et les données ",
    "utilisées pour cette page, visitez la publication <a href = 'https://f",
    "r.davidsuzuki.org/publication-scientifique/le-role-des-infrastructures",
    "-naturelles-dans-la-prevention-des-inondations-dans-la-communaute-metr",
    "opolitaine-de-montreal/' target = '_blank'>Le rôle des infrastructures",
    " naturelles dans la prévention des inondations dans la Communauté métr",
    "opolitaine de Montréal</a>.</p>"
  )) |> 
  add_row(en = "<p>Montrea's green alleys have been classified into four types: green alleys, community-oriented alleys, mixed alleys, and unmaintained alleys. The datasets visualized on this page come from the City of Montreal Open Data Portal and Curbcut. To learn more about the Green Alley Program in Montreal, visit <a href = ‘https://montreal.ca/en/topics/green-alleyways’ target = ‘_blank’>the City’s green alleyways page</a>. ",
          fr = "Les ruelles vertes de Montréal ont été classées en quatre types : ruelles vertes, ruelles axées sur la communauté, ruelles mixtes et ruelles non entretenues. Les ensembles de données visualisés sur cette page proviennent du Portail de données ouvertes de la Ville de Montréal et de Curbcut. Pour en savoir plus sur le Programme des ruelles vertes à Montréal, visitez <a href = 'https://montreal.ca/sujets/ruelles-vertes' target = '_blank'>la page des ruelles vertes de la Ville</a>.</p>") |> 
  add_row(en = "<p>Data is collected by the Service de Police de la Ville de Montréal (SPVM) and compiled by the Société d’Assurance Automobile du Québec (SAAQ), and contains information related to every road collision, including the date, location and type of parties involved (i.e. cars, bicycles or pedestrians) and injury severity. <p>For more information on road collisions and a temporal analysis of the data, please consult the <i>Road safety analysis</i> below.</p><p>References:</p><ul><li><a href = https://www.pietons.quebec/sites/default/files/documents/pietonsqc_vf_fiche_decouvrirapprochevisionzerosecuriteroutiere.pdf> Piétons Québec. (2021). Découvrir l’approche vision zéro en sécurité routière. Piétons Québec. Online:</a><li><a href='https://donnees.montreal.ca/ville-de-montreal/collisions-routieres'>Ville de Montréal. (2021). Collisions routières. Données Ouvertes Montréal.</a></ul>",
          fr = paste0(
            "Les données sont collectées par le Service de Police de la Ville de Mo",
            "ntréal (SPVM) et compilées par la Société d'Assurance Automobile du Qu",
            "ébec (SAAQ). Elles contiennent des informations relatives à chaque col",
            "lision routière, y compris la date, le lieu et le type de parties impl",
            "iquées (c'est-à-dire voitures, vélos ou piétons) ainsi que la gravité ",
            "des blessures.</p><p>Pour plus d'informations sur les collisions routi",
            "ères et une analyse temporelle des données, veuillez consulter l'<i>an",
            "alyse de la sécurité routière</i> ci-dessous.</p><p>Références :</p><u",
            "l><li><a href='https://www.pietons.quebec/sites/default/files/document",
            "s/pietonsqc_vf_fiche_decouvrirapprochevisionzerosecuriteroutiere.pdf'>",
            "Piétons Québec. (2021). Découvrir l'approche vision zéro en sécurité r",
            "outière. Piétons Québec. En ligne :</a></li><li><a href='https://donne",
            "es.montreal.ca/ville-de-montreal/collisions-routieres'>Ville de Montré",
            "al. (2021). Collisions routières. Données Ouvertes Montréal.</a></li><",
            "/ul>"
          ))
  
  



