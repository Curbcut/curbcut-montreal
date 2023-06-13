# library(tibble)
# 
# # Translation function
# .deepl_key <- "42c0646a-5ebd-ada0-07a0-d27c8eb37613:fx"
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
# strings <- modules$title_text_extra
# 
# translation_fun(strings)

#TITLES

translation_pages <- 
  tibble(en = character(),
         fr = character()) |> 
  add_row(en = "Housing system", 
          fr = "Le système de logement") |>
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

#TEXT

add_row(en = "<p>Housing is at the centre of our lives. Our ability to find affordable, adequate, and healthy accommodations profoundly affects our life chances.", 
        fr = "<p>Le logement est au cœur de nos vies. Notre capacité à trouver un logement abordable, adéquat et sain affecte profondément nos chances dans la vie.") |>
  add_row(en = "<p>Information about vacancy rates can help define past and current trends in the housing market and what is needed to better provide adequate rental housing.", 
          fr = "<p>Les informations sur les taux d'inoccupation peuvent aider à définir les tendances passées et actuelles du marché du logement et à déterminer ce qui est nécessaire pour mieux fournir des logements locatifs adéquats.") |>
  add_row(en = "<p>The walkability of an area is influenced by both the built environment and socio-economic factors. The Active Living Potential index quantifies which areas provide walkable environments to their residents.", 
          fr = "<p>Le potentiel piétonnier d'une zone est influencée à la fois par l'environnement bâti et par des facteurs socio-économiques. L'indice du potentiel de vie active quantifie les zones qui offrent un environnement propice à la marche à leurs habitants.") |>
  add_row(en = "<p>The time and mode of transportation needed to reach amenities plays a large role in daily experiences and quality of life. Understanding access to amenities by mode of transportation gives a glimpse into how different areas are serviced and what that might imply for residents. ", 
          fr = "<p>Le temps et le mode de transport nécessaires pour accéder des services et commodités jouent un rôle important dans les expériences quotidiennes et la qualité de vie. Comprendre l'accès aux services et commodités par mode de transport donne un aperçu de la manière dont les différentes zones sont desservies et de ce que cela peut impliquer pour les résidents.") |>
  add_row(en = "<p>Climate change will have increasingly negative impacts on communities in Montreal, but these will vary significantly by both geographic and social factors. The distribution of five different climate risks – heat waves, flooding, heavy rain, drought, and destructive storms – is visualized here.", 
          fr = "<p>Les changements climatiques auront des impacts de plus en plus négatifs sur les communautés de Montréal, mais ceux-ci varieront considérablement en fonction des facteurs géographiques et sociaux. La distribution de cinq risques climatiques différents - vagues de chaleur, crues, pluies abondantes, sécheresse et tempêtes destructrices - est visualisée ici.") |>
  add_row(en = "<p>Natural ecosystems are necessary for our cities, they help contribute to well-being, quality of life and public health. This page quantifies the benefits provided by urban trees and wooded areas to biodiversity conservation, flood prevention, and heat-island reduction. ", 
          fr = "<p>Les écosystèmes naturels sont nécessaires à nos villes, ils contribuent au bien-être, à la qualité de vie et à la santé publique. Cette page quantifie les bénéfices apportés par les arbres urbains et les zones boisées à la conservation de la biodiversité, à la prévention des inondations et à la réduction des îlots de chaleur.") |>
  add_row(en = "<p>Green alleys are spaces that have been transformed by residents for their own activities. When adequately designed, these public spaces can help reduce heat island effects, noise, and air pollution. These alleys have been classified into four types: green alleys, community-oriented alleys, mixed alleys, and unmaintained alleys.", 
          fr = "<p>Les ruelles vertes sont des espaces transformés par les habitants pour leurs propres activités. Lorsqu'ils sont bien conçus, ces espaces publics peuvent contribuer à réduire l'effet d'îlot de chaleur, le bruit et la pollution de l'air. Ces ruelles ont été classées en quatre types : les ruelles vertes, les ruelles communautaires, les ruelles mixtes et les ruelles non entretenues.") |>
  add_row(en = "<p>Access to affordable and equitable housing is a fundamental human right, yet it remains a pressing challenge for many communities. While affordable housing is often broadly defined as spending less than 30% of household income on shelter, the reality for each household is much more complex.", 
          fr = "<p>L'accès à un logement abordable et équitable est un droit humain fondamental, mais il reste un défi pressant pour de nombreuses communautés. Un logement abordable est souvent défini de manière générale comme le fait de consacrer moins de 30 % du revenu d'un ménage au logement, mais la réalité de chaque ménage est beaucoup plus complexe.") |>
  add_row(en = "<p>Road safety is an important consideration for wellbeing and safety in cities. This page provides an overview and analysis of road collisions in the City of Montreal, ranging from 2012 to today.", 
          fr = "<p>La sécurité routière est une considération importante pour le bien-être et la sécurité dans les villes. Ce module fournit une vue d'ensemble et une analyse des collisions routières dans la ville de Montréal, de 2012 à aujourd'hui.") |>
  add_row(en = "<p>Select a location by entering a postal code or clicking on the map and see how it compares to the rest of the Montreal region or island across a variety of sustainability indicators.", 
          fr = "<p>Sélectionnez un lieu en entrant un code postal ou en cliquant sur la carte et voyez comment il se compare au reste de la région ou de l'île de Montréal selon divers indicateurs de durabilité.") |>
  add_row(en = "<p>Explore stories about urban sustainability and planning in Montreal. Learn about stories rooted in specific geographic locations or those that have an impact on the whole city.", 
          fr = "<p>Explorez les histoires sur la durabilité et la planification urbaine à Montréal. Découvrez des histoires enracinées dans des lieux géographiques spécifiques ou celles qui ont un impact sur l'ensemble de la ville.") |> 
  add_row(en = paste0(
    "<p>Understanding housing needs by tenure status can help to inform ",
    "what is to be improved specifically for tenants or owners as they ",
    "might be experiencing different difficulties and advantages."
  ), fr = "<p>La compréhension des besoins en matière de logement en fonction du statut d'occupation peut aider à déterminer ce qui doit être amélioré spécifiquement pour les locataires ou les propriétaires, étant donné qu'ils peuvent rencontrer des difficultés et des avantages différents.")

#LEARN MORE 

add_row(en = "<p>The datasets visualized on this page come from the Canadian Census from 1996 to the present. There are a few efforts in place to better the housing landscape from the federal and municipal governments. In Canada, the National Housing Strategy aims to address housing needs and houselessness through modernization, new construction, and innovation and research. Within the City of Montreal, important housing initiatives include the Diverse Metropolis by-law and the 12,000-housing unit strategy. For more information on these initiatives visit:<ul><li><a href='https://www.cmhc-schl.gc.ca/en/nhs/', target = '_blank'>CMHC. (n.d.). National Housing Strategy</a><li><a href='https://montreal.ca/articles/metropole-mixte-les-grandes-lignes-du-reglement-7816', target = '_blank'>Ville de Montréal. (4 octobre 2021). Métropole Mixte: Les grandes lignes du règlement.</a>", 
        fr = "<p>Les ensembles de données visualisés sur cette page proviennent du recensement canadien de 1996 à aujourd'hui. Les gouvernements fédéral et municipaux ont déployé quelques efforts pour améliorer le paysage du logement. Au Canada, la stratégie nationale du logement vise à répondre aux besoins en matière de logement et à lutter contre le sans-abrisme par la modernisation, la construction de nouveaux logements, l'innovation et la recherche. Au sein de la ville de Montréal, les initiatives importantes en matière de logement comprennent le règlement Métropole mixte et la stratégie de développment de 12 000 logements. Pour plus d'informations sur ces initiatives, visitez :<ul><li><a href='https://www.cmhc-schl.gc.ca/en/nhs/', target = '_blank'>CMHC. (s.d.). Stratégie nationale sur le logement</a><li><a href='https://montreal.ca/articles/metropole-mixte-les-grandes-lignes-du-reglement-7816', target = '_blank'>Ville de Montréal. (4 octobre 2021). Métropole Mixte : Les grandes lignes du règlement.</a>") |>
  add_row(en = "<p>The datasets visualized on this page come from the CMHC and the 2021 Canadian Census.", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent de la SCHL et du recensement canadien de 2021.") |>
  add_row(en = "<p>The datasets visualized on this page come from Curbcut using data from the Canadian Censuses and DMTI. Our index considers street connectivity, building density, and points of interest. Active Living Potential is then calculated based on dissemination areas accessible within a 15-minute walk. The work on this page was highly influenced by the <a href = 'http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf', target = '_blank'>CanALE index</a> developed by Prof. Nancy Ross and her team.", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent de Curbcut, à partir des données des recensements canadiens et de DMTI. Notre indice tient compte de la connectivité des rues, de la densité des bâtiments et des points d'intérêt. Le potentiel de vie active est ensuite calculé en fonction des aires de diffusion accessibles en moins de 15 minutes de marche. Le travail sur cette page a été fortement influencé par le <a href = 'http://canue.ca/wp-content/uploads/2018/03/CanALE_UserGuide.pdf', target = '_blank'>CanALE index</a> développé par la professeur Nancy Ross et son équipe.") |>
  add_row(en = "<p>Curbcut has calculated travel times for walking, cycling, and driving using the Open Source Routing Machine (OSRM) and the OpenStreetMap (OSM) street network. For transit travel times, Curbcut has employed GTFS feeds and a multimodal approach, incorporating walking times derived from OSRM and the OSM street network. The amenities data has been sourced from a combination of DMTI Spatial and OpenStreetMap.", 
          fr = "<p>Curbcut a calculé les temps de parcours pour la marche, le vélo et la voiture en utilisant l'Open Source Routing Machine (OSRM) et le réseau de rues OpenStreetMap (OSM). Pour les temps de trajet en transport en commun, Curbcut a utilisé les flux GTFS et une approche multimodale, en incorporant les temps de marche dérivés de l'OSRM et du réseau de rues OSM. Les données sur les aménagements proviennent d'une combinaison de DMTI Spatial et d'OpenStreetMap.") |>
  add_row(en = "<p>The datasets visualized on this page are publicly available through the <a href = ‘https://donnees.montreal.ca/dataset/vulnerabilite-changements-climatiques’ target = ‘_blank’>Montreal Open Data Portal</a>. These were developed as part of the City of Montreal’s efforts to examine potential climate risks for the Montreal region in the <a href = ‘Climate change adaptaiton plan for the Montréal Urban Agglomeration, 2017 edition (montreal.qc.ca)’ target = ‘_blank’>2015-2020 Urban Agglomeration Climate Change Adaptation Plan</a>.", 
          fr = "<p>Les ensembles de données visualisés sur cette page sont accessibles au public via le <a href = 'https://donnees.montreal.ca/dataset/vulnerabilite-changements-climatiques' target = '_blank'>Portail de données ouvertes de Montréal</a>. Ils ont été développés dans le cadre des efforts de la Ville de Montréal pour examiner les risques climatiques potentiels pour la région de Montréal dans le <a href = 'Plan d'adaptation aux changements climatiques de l'agglomération de Montréal, édition 2017 (montreal.qc.ca)' target = '_blank'>Plan d'adaptation aux changements climatiques de l'agglomération de Montréal 2015-2020</a>.") |>
  add_row(en = "<p>The datasets visualized on this page come from Habitat. Note that the natural infrastructure included in the study that generated this data only covers approximately 25% of the Montreal region. For more information on the methods and data used for this page, visit the publication <a href = ‘https://fr.davidsuzuki.org/publication-scientifique/le-role-des-infrastructures-naturelles-dans-la-prevention-des-inondations-dans-la-communaute-metropolitaine-de-montreal/ ‘ target = ‘_blank’>“Le rôle des infrastructures naturelles dans la prévention des inondations dans la Communauté métropolitaine de Montréal”</a>.", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent d'Habitat. Il est à noter que les infrastructures naturelles incluses dans l'étude qui a généré ces données ne couvrent qu'environ 25 % de la région de Montréal. Pour plus d'information sur les méthodes et les données utilisées pour cette page, visitez la publication <a href = 'https://fr.davidsuzuki.org/publication-scientifique/le-role-des-infrastructures-naturelles-dans-la-prevention-des-inondations-dans-la-communaute-metropolitaine-de-montreal/ ' target = '_blank'>Le rôle des infrastructures naturelles dans la prévention des inondations dans la Communauté métropolitaine de Montréal</a>.") |>
  add_row(en = "<p>The datasets visualized on this page come from the City of Montreal Open Data Portal, and Curbcut. To learn more about the Green Alley Program in Montreal, visit <a href = ‘https://montreal.ca/en/topics/green-alleyways’ target = ‘_blank’>the city’s green alleyways page</a>. ", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent du Portail de données ouvertes de la Ville de Montréal et de Curbcut. Pour en savoir plus sur le programme des ruelles vertes à Montréal, visitez <a href = 'https://montreal.ca/en/topics/green-alleyways' target = '_blank'>la page des ruelles vertes de la ville</a>.") |>
  add_row(en = "<p>The datasets visualized on this page come from the 2016 and 2021 Canadian Censuses.", 
          fr = "<p>Les ensembles de données visualisés sur cette page proviennent des recensements canadiens de 2016 et de 2021.") |>
  add_row(en = "<p>Data is collected by the Service de Police de la Ville de Montréal (SPVM) and compiled by the Société d’Assurance Automobile du Québec (SAAQ), and contains information related to every road collision, including the date, location and type of parties involved (i.e. cars, bicycles or pedestrians) and injury severity. <p>For more information on road collisions and a temporal analysis of the data, please consult the <i>Road safety analysis</i> below.</p><p>References:</p><ul><li><a href = https://www.pietons.quebec/sites/default/files/documents/pietonsqc_vf_fiche_decouvrirapprochevisionzerosecuriteroutiere.pdf> Piétons Québec. (2021). Découvrir l’approche vision zéro en sécurité routière. Piétons Québec. Online:</a><li><a href='https://donnees.montreal.ca/ville-de-montreal/collisions-routieres'>Ville de Montréal. (2021). Collisions routières. Données Ouvertes Montréal.</a></ul>", 
          fr = "<p>Les données sont recueillies par le Service de police de la Ville de Montréal (SPVM) et compilées par la Société d'assurance automobile du Québec (SAAQ). Elles contiennent des informations relatives à chaque collision routière, notamment la date, le lieu et le type de personnes impliquées (voitures, vélos ou piétons) ainsi que la gravité des blessures. <Pour plus d'information sur les collisions routières et une analyse temporelle des données, veuillez consulter l'<i>Analyse de la sécurité routière</i> ci-dessous.</p><p>Références:</p><ul><li><a href = https://www.pietons.quebec/sites/default/files/documents/pietonsqc_vf_fiche_decouvrirapprochevisionzerosecuriteroutiere.pdf> Piétons Québec. (2021). Découvrir l'approche vision zéro en sécurité routière. Piétons Québec. En ligne:</a><li><a href='https://donnees.montreal.ca/ville-de-montreal/collisions-routieres'>Ville de Montréal. (2021). Collisions routières. Données Ouvertes Montréal.</a></ul>") |>
  add_row(en = "<p>The data in the Place Explorer is taken from other Curbcut pages with two exceptions: <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>Air pollution</a> and <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_GRAVH_AMN_YY.pdf'>green space</a> data are taken from <a href = 'https://www.canuedata.ca'>CANUE</a>.", 
          fr = "<p>Les données de l'explorateur de lieux proviennent d'autres pages de Curbcut, à deux exceptions près : <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf'>Pollution de l'air</a> et <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA_GRAVH_AMN_YY.pdf'>espace vert</a> proviennent de <a href = 'https://www.canuedata.ca'>CANUE</a>.") |>
  add_row(en = "<p>These narrative case studies are written by the Curbcut team and its contributors.", 
          fr = "<p>Ces études de cas narratives sont rédigées par l'équipe de Curbcut et ses contributeurs.")