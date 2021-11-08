#### Translation data setup ####################################################

library(tidyverse)
library(qs)

translation_fr <- 
  tibble(en = character(), fr = character()) %>% 
  
  # Menu
  add_row(en = "Biodiversity", fr = "Biodiversité") %>%
  add_row(en = "Learn more", fr = "En savoir plus") %>%
  add_row(en = "Compare", fr = "Comparez") %>%
  add_row(en = "SUS Preview", fr = "Aperçu du SUS") %>%
  
  # Module names
  add_row(en = "Home", fr = "Accueil") %>%
  add_row(en = "Climate change risk", fr = "Vulnérabilité aux changements climatiques") %>%
  add_row(en = "Climate change", fr = "Changements climatiques") %>%
  add_row(en = "Covid-19", fr = "Covid-19") %>%
  add_row(en = "Transport", fr = "Transport") %>%
  add_row(en = "Ecology", fr = "Écologie") %>%
  add_row(en = "Urban life", fr = "La vie urbaine") %>%
  add_row(en = "Accessibility to urban opportunities", fr = "Accessibilité aux opportunités urbaines") %>%
  add_row(en = "Place explorer", fr = "Explorez un lieu") %>%
  
  # title_text.csv
  add_row(en = "Why a dashboard? The science behind Sus", 
          fr = "Pourquoi un tableau de bord? La science derrière Sus") %>%
  add_row(
    en = paste0(
      "The CanALE dataset (developed by Prof. Nancy Ross and her team) captures ",
      "four key elements related to active living environments: population density, ",
      "points of interest, street grid, and proximity of transit service."), 
    fr = paste0(
      "La base de données AVA-Can (développée par la professeure Nancy Ross et son ",
      "équipe) saisit quatre éléments clés liés aux environnements de vie active: ",
      "la densité de population, les points d'intérêt, la grille des rues et la proximité ",
      "du service de transport en commun.")) %>%
  add_row(
    en = paste0(
      "<p>A safe and inviting pedestrian realm is not distributed equally ", 
      "across socio-demographic factors. The risks of pedestrian injuries and ", 
      "fatalities are higher in low-income and racialized communities ",
      "where residents often rely on walking as a daily mode of transport but ", 
      "where the local environment is not necessarily inviting and safe.<p>In ",
      "addition to evidence pointing towards large discrepancies ",
      "in the provision of walkable urban space across income and racial lines, ",
      "concern has been raised with regard to the possible gentrification and ", 
      "displacement impacts of improved pedestrian infrastructure. ",
      "In other words, who can afford to live in walkable ", 
      "neighbourhoods?<br><p>Further resources:<ul><li><a href= ''>Thomas ", 
      "Herrmann, William Gleckner, Rania A. Wasfi, Benoît Thierry, Yan ",
      "Kestens and Nancy A. Ross. 2019. 'A pan-Canadian measure of active ", 
      "living environments using open data. Statistics Canada Health Reports, ",
      "82-003-X.</a><li><a href = ''>Kevin Manaugh, Linnea Soli, Samuel Kohn, ",
      "Robin Basalaev-Binder, Ty Tuff, David Wachsmuth. 2020. 'Montreal’s ", 
      "response to COVID-19: An equity analysis of new active transport ", 
      "infrastructure.' Transportation Research Board working paper.</a> ", 
      "<b>(MSSI research)</b></ul><br><p><i>Module ",
      "lead authors: Robin Basalaev-Binder, David Wachsmuth</i>"), 
    fr = paste0(
      "<p>Une voie piétonne sécuritaire et accueillante n'est pas répartie de manière égale entre les ",
      "facteurs sociodémographiques. Les risques de blessures et de décès de piétons sont plus élevés ",
      "dans les communautés racialisées et à faible revenu, où les résidents comptent souvent sur la ",
      "marche comme mode de transport quotidien, mais où l'environnement local n'est pas nécessairement ",
      "accueillant et sécuritaire.<p>Outre les éléments indiquant de grandes disparités dans la provision ",
      "d'un espace piétonnier urbain selon le revenu et le pourcentage de ", 
      "minorités visibles, des préoccupations ont été soulevées ",
      "concernant les éventuels impacts de l'amélioration des infrastructures piétonnes sur la gentrification ",
      "et le déplacement. En d'autres termes, qui peut se permettre de vivre dans des quartiers piétonniers?",
      "<br><p>Ressources supplémentaires:<ul><li><a href= ''>Thomas Herrmann, William Gleckner, Rania A. Wasfi, ",
      "Benoît Thierry, Yan Kestens et Nancy A. Ross. 2019. Une mesure ", 
      "pancanadienne fondées sur les données ouvertes de ",
      "l'accessibilité à la vie active dans les milieux de vie. Rapports sur ", 
      "la santé de Statistique Canada, 82-003-X.</a><li><a ",
      "href = ''>Kevin Manaugh, Linnea Soli, Samuel Kohn, Robin Basalaev-Binder, Ty Tuff, David Wachsmuth. ",
      "2020. «Réponse de Montréal à COVID-19 : Une analyse de l'équité des nouvelles infrastructures de ",
      "transport actif». Document de travail pour le Transportation Research ",
      "Board.</a> <b>(MSSI research)</b></ul><br><p><i>Auteur(e)s ", 
      "principaux(ales) du module: Robin Basalaev-Binder, David Wachsmuth</i>")) %>%
  add_row(
    en = paste0(
      "Based on 2016 commuting data, we quantify possible reductions in VMT and GHG by identifying car ",
      "trips that could be shifted to cycling based on distance, elevation change and other factors."), 
    fr = paste0(
      "Basé sur les données relatives aux déplacements domicile-travail en 2016, nous quantifions ",
      "les réductions possibles de KPV et de GES en identifiant les déplacements en voiture qui pourraient ",
      "être remplacés par des déplacement en vélo en fonction de la distance, du changement ",
      "d'altitude et d'autres facteurs.")) %>%
  add_row(
    en = paste0(
      "<p>The transportation sector is a major contributor to Canada's greenhouse gas (GHG) emissions which ",
      "are linked to global climate change. Reducing vehicle miles traveled (VMT) over the long term is ",
      "increasingly recognized as the key to reduce GHG emissions from the transportation sector but has not ",
      "received as much attention as needed.<p>The primary objective of this study is to investigate the ",
      "potential for reducing VMT and GHG emissions by shifting short car trips to cycling in Montreal. ",
      "Based on commuting data from the 2016 Canadian Census, commuting patterns were explored. Two scenarios ",
      "were introduced to model environmental effects of a modal shift towards cycling based on characteristics ",
      "of current bicycle trips.<p>The results showed that enhanced cycling commuting can reduce VMT and GHG ",
      "emissions from car travel. Other mitigation measures are necessary for achieving GHG emissions reduction ",
      "targets.<br><p><i>Module lead authors: Qiao Zhao, Kevin Manaugh</i>"), 
    fr = paste0(
      "<p>Le secteur des transports est un contributeur majeur aux émissions de gaz à effet de serre (GES) ",
      "du Canada, qui sont liées au changement climatique mondial. La réduction des kilomètres parcourus ",
      "par les véhicules (KPV) sur le long terme est de plus en plus reconnue comme la clé de la réduction ",
      "des émissions de GES du secteur des transports, mais n'a pas reçu autant d'attention que nécessaire.<p>Le ",
      "principal objectif de cette étude est d'étudier le potentiel de réduction des KPV et des émissions de ",
      "GES en déplaçant les courts trajets en voiture par le vélo à Montréal. ", 
      "Deux scénarios ont été introduits pour modéliser ",
      "les effets environnementaux d'un transfert modal vers le vélo en fonction des caractéristiques des ",
      "déplacements actuels à vélo.<p>Les résultats ont montré que l'augmentation des déplacements à vélo peut ",
      "réduire les KPV et les émissions de GES des déplacements en voiture. D'autres mesures de réduction ",
      "sont nécessaires pour atteindre les objectifs de diminution des émissions de GES.<br><p><i>Auteurs ",
      "principaux du module: Qiao Zhao, Kevin Manaugh</i>")) %>%
  add_row(
    en = paste0(
      "The capacity for pedestrian social distancing is a capacity measurement that determines the percentage ",
      "of a neighbourhood’s population that can make local trips on foot at the same time while respecting ",
      "‘social distancing’ regulations."), # I don't believe we need to put social distancing in quotes
    fr = paste0(
      "La capacité de distanciation sociale des piétons est une mesure de capacité qui détermine le pourcentage ",
      "de la population d'un quartier qui peut faire des trajets locaux à pied en même temps, tout en respectant ",
      "les règles de «distanciation sociale».")) %>%
  add_row(
    en = paste0(
      "<p>Using open data from Montreal's open data portal as well as OpenStreetMap, it was possible to calculate ",
      "the total surface area of sidewalks, neighbourhood parks, and pre-Covid pedestrian streets. Summing these ",
      "surface areas gets us the neighbourhood's total walkable surface area. ",
      "It is then possible to calculate how ",
      "many residents can ‘fit’ into the pedestrian realm while respecting ", 
      "‘social distancing’ regulations of 2 meters ",
      "(total walkable surface area divided by the surface area of a circle with a 2-meter radius, that is 12.54 ",
      "square meters). Finally, we normalize the value by representing it as ", 
      "a percentage of the residential population. ",
      "Neighbourhoods where less than 100% of the local population can make ",
      "trips on foot at the same time are above ",
      "capacity and more at risk of overcrowding from local pedestrian trips. <p>While it is still important to ",
      "take into account pedestrian flows coming from external neighbourhoods ",
      "(some data on pedestrian flows obtained ",
      "from 2016 TrajetMtl data is presented in this research), the ",
      "measurement’s focus on local pedestrian capacity ",
      "is especially relevant during a pandemic situation where shelter in ",
      "place and travel restrictions have ",
      "generally led to a rise in local trips and a decline in trips from other neighbourhoods.<br><p><i>Module ",
      "lead author: Samuel Kohn</i>"), 
    fr = paste0(
      "<p>En utilisant les données du portail de données ouvertes de Montréal ainsi que OpenStreetMap, il a été ",
      "possible de calculer la surface totale des trottoirs, des parcs de quartier et des rues piétonnes pré-Covid-19. ",
      "En additionnant ces surfaces, on obtient la surface totale du quartier où l'on peut marcher. Il est alors ",
                      "possible de calculer combien d'habitants peuvent «s'intégrer» dans la zone piétonne tout en respectant ",
                      "les règles de «distanciation sociale» de 2 mètres (surface totale piétonne divisée par la surface d'un ",
                      "cercle de 2 mètres de rayon, soit 12,54 mètres carrés). Enfin, nous normalisons la valeur en la représentant ",
                      "en pourcentage de la population résidentielle. Les quartiers où moins de 100 % de la population locale peut ",
                      "effectuer des trajets à pied en même temps sont au-dessus de leur capacité et sont plus exposés au risque de ",
                      "surpeuplement des trajets locaux à pied. <p>Même s'il est toujours important de prendre en compte les flux ",
                      "de piétons provenant des quartiers extérieurs (certaines données sur les flux de piétons obtenues à partir ",
                      "des données de 2016 de TrajetMtl sont présentées dans cette recherche), l'accent mis par la mesure sur la ",
                      "capacité piétonne locale est particulièrement pertinent en situation de pandémie, où les protections en place ",
                      "et les restrictions de déplacement ont généralement conduit à une augmentation des déplacements locaux et à ",
                      "une diminution des déplacements provenant d'autres quartiers.<br><p><i>Auteur principal du module: ",
                      "Samuel Kohn</i>")) %>%
  add_row(en = paste0("Compare the pedestrian capacity for social distancing metric across a variety of other variables, such ",
                      "as walkable access to amenities, income level, immigration, visible minorities, population density, etc. "),
          fr = paste0("Comparez la mesure de la capacité des piétons à pratiquer la distance sociale en fonction de diverses ",
                      "autres variables, telles que l'accès à pied aux services de base, le revenu, l'immigration, les ",
                      "minorités visibles, la densité de population, etc.")) %>%
  add_row(en = paste0("The data shows us that the ability to safely navigate pedestrian space tends to be much lower in ",
                      "DAs with lower incomes as well as DAs with high proportions of visible minorities and immigrants ",
                      "compared to majority white regions of Montreal. While the City’s plans to increase walkable urban ",
                      "space made some improvements to these discrepancies, there is room for improvement. The interactive ",
                      "data within this platform has the potential to support policy-makers towards making strategic decisions ",
                      "with more equitable outcomes. Below is a policy analysis exemplar using two variables: capacity for pedestrian ",
                      "social distancing and walkable access to key amenities."),
          fr = paste0("Les données nous montrent que la capacité à se déplacer en toute sécurité dans l'espace piétonnier ",
                      "tend à être beaucoup plus faible dans les AD à faible revenu ainsi que dans les AD ayant une forte ",
                      "proportion de minorités visibles et d'immigrants par rapport aux régions de Montréal où la majorité ",
                      "des habitants sont blancs. Bien que les plans de la Ville visant à augmenter l'espace urbain piétonnier ",
                      "aient permis d'améliorer ces écarts, il y a place à amélioration. Les données interactives de cette ",
                      "plateforme peuvent aider les décideurs politiques à prendre des décisions stratégiques avec des résultats ",
                      "plus équitables. Vous trouverez ci-dessous un exemple d'analyse de politique utilisant deux variables: ",
                      "la capacité de distanciation sociale des piétons et l'accès à pied aux services de base.")) %>%
  add_row(en = paste0("In order to calculate the width of sidewalks in Montreal, we used a spatial dataset published by ",
                      "Montréal Open Data Portal that includes polygons of all sidewalks within the Montreal agglomeration. ",
                      "Using the the object-oriented programming language R, we developed a function which first creates ",
                      "negative buffers inside each sidewalk segment, and then iteratively adjusts the distance of that buffer ",
                      "until the maximum distance is achieved which still produces valid buffer geometry (if the buffer ",
                      "boundaries overlap, the geometry becomes invalid). The outcome is the equivalent of a centreline ",
                      "inside each sidewalk polygon. The last step to determine sidewalk width is to sum the distances ",
                      "between the centreline and both edges of a given sidewalk polygon segment. This process is ",
                      "illustrated below."),
          fr = paste0("Afin de calculer la largeur des trottoirs à Montréal, nous avons utilisé un ensemble de données ",
                      "spatiales publié par le Portail de données ouvertes de Montréal, qui comprend les polygones de ",
                      "tous les trottoirs de l'agglomération de Montréal. En utilisant le langage de programmation orienté ",
                      "objet R, nous avons développé une fonction qui crée d'abord des tampons négatifs à l'intérieur de ",
                      "chaque segment de trottoir, puis ajuste itérativement la distance de ce tampon jusqu'à ce que la ",
                      "distance maximale soit atteinte, ce qui produit encore une géométrie de tampon valide (si les ",
                      "limites du tampon se chevauchent, la géométrie devient invalide). Le résultat est l'équivalent ",
                      "d'une ligne centrale à l'intérieur de chaque polygone de trottoir. La dernière étape pour déterminer ",
                      "la largeur du trottoir consiste à additionner les distances entre la ligne centrale et les deux bords ",
                      "d'un segment donné de polygone de trottoir. Ce processus est illustré ci-dessous.")) %>%
  
  # var_exp$var_name
  
  # List of variables automatically translated from Deepl. It seems however that
  # all variables have already been all translatate under.
  
  # add_row(en = "Tenant-occupied (%)",
  #         fr = "Locataire-occupant (%)") %>% 
  # add_row(en = "Average rent ($)",
  #         fr = "Loyer moyen ($)") %>% 
  # add_row(en = "Average property value ($)",
  #         fr = "Valeur moyenne des propriétés ($)") %>% 
  # add_row(en = "Unaffordable housing (%)",
  #         fr = "Logement inabordable (%)") %>% 
  # add_row(en = "Unsuitable housing (%)",
  #         fr = "Logement inadapté (%)") %>% 
  # add_row(en = "Housing requiring major repairs (%)",
  #         fr = "Logements nécessitant des réparations importantes (%)") %>% 
  # add_row(en = "Owner housing stress (%)",
  #         fr = "Stress lié au logement des propriétaires (%)") %>% 
  # add_row(en = "Renter housing stress (%)",
  #         fr = "Stress lié au logement des locataires (%)") %>% 
  # add_row(en = "One-year housing mobility (%)",
  #         fr = "Mobilité du logement sur un an (%)") %>% 
  # add_row(en = "Five-year housing mobility (%)",
  #         fr = "Mobilité du logement sur cinq ans (%)") %>% 
  # add_row(en = "Median household income ($)",
  #         fr = "Revenu médian des ménages ($)") %>% 
  # add_row(en = "Income under $50k (%)",
  #         fr = "Revenu inférieur à 50 000 $ (%)") %>% 
  # add_row(en = "Income beetween $50k-$100k (%)",
  #         fr = "Revenu compris entre 50 000 et 100 000 dollars (%)") %>% 
  # add_row(en = "Income above $100k (%)",
  #         fr = "Revenu supérieur à 100 000 dollars (%)") %>% 
  # add_row(en = "Prevalence of low income (after-tax) (%)",
  #         fr = "Prévalence des faibles revenus (après impôts) (%)") %>% 
  # add_row(en = "Immigrants (%)",
  #         fr = "Immigrants (%)") %>% 
  # add_row(en = "New immigrants (%)",
  #         fr = "Nouveaux immigrants (%)") %>% 
  # add_row(en = "Visible minorities (%)",
  #         fr = "Minorités visibles (%)") %>% 
  # add_row(en = "Aboriginal (%)",
  #         fr = "Autochtones (%)") %>% 
  # add_row(en = "Drive to work (%)",
  #         fr = "Se rendre au travail en voiture (%)") %>% 
  # add_row(en = "Walk or cycle to work (%)",
  #         fr = "Se rendre au travail à pied ou à vélo (%)") %>% 
  # add_row(en = "Public transit to work (%)",
  #         fr = "Transport en commun pour se rendre au travail (%)") %>% 
  # add_row(en = "15 minutes to work (%)",
  #         fr = "15 minutes pour aller travailler (%)") %>% 
  # add_row(en = "15-45 minutes to work (%)",
  #         fr = "15-45 minutes pour aller travailler (%)") %>% 
  # add_row(en = "More than 45 minutes to work (%)",
  #         fr = "Plus de 45 minutes pour aller travailler (%)") %>% 
  # add_row(en = "Managerial and professional occupations (%)",
  #         fr = "Cadres et professions libérales (%)") %>% 
  # add_row(en = "Creative occupations (%)",
  #         fr = "Professions créatives (%)") %>% 
  # add_row(en = "Families with children (%)",
  #         fr = "Familles avec enfants (%)") %>% 
  # add_row(en = "One person households (%)",
  #         fr = "Ménages d'une personne (%)") %>% 
  # add_row(en = "French only (%)",
  #         fr = "Français seulement (%)") %>% 
  # add_row(en = "English only (%)",
  #         fr = "Anglais seulement (%)") %>% 
  # add_row(en = "French and English (%)",
  #         fr = "Français et anglais (%)") %>% 
  # add_row(en = "Neither French nor English (%)",
  #         fr = "Ni français ni anglais (%)") %>% 
  # add_row(en = "Aged between 0 and 14 (%)",
  #         fr = "Âgés de 0 à 14 ans (%)") %>% 
  # add_row(en = "Aged between 15 and 64 (%)",
  #         fr = "Âgés entre 15 et 64 ans (%)") %>% 
  # add_row(en = "Aged 65 and above (%)",
  #         fr = "Agés de 65 ans et plus (%)") %>% 
  # add_row(en = "Bachelor and above (%)",
  #         fr = "Baccalauréat et plus (%)") %>% 
  # add_row(en = "No certificate, diploma or degree (%)",
  #         fr = "Aucun certificat, diplôme ou grade (%)") %>% 
  # add_row(en = "CanALE index",
  #         fr = "Indice CanALE") %>% 
  
  # var_exp$explanation
  add_row(en = "the percentage of private dwellings occupied by tenants",
          fr = "le pourcentage de logements privés occupés par des locataires") %>% 
  add_row(en = "the average rent paid by tenants per month",
          fr = "le loyer moyen payé par les locataires par mois") %>% 
  add_row(en = "the average value of owner-occupied dwellings",
          fr = "la valeur moyenne des logements occupés par leur propriétaire") %>% 
  add_row(en = "the percentage of dwellings for which residents pay more than 30% of income on housing costs",
          fr = "le pourcentage de logements pour lesquels les résidents consacrent plus de 30 % de leurs revenus aux frais de logement") %>% 
  add_row(en = "the percentage of households living in accommodations without enough bedrooms according to the National Occupancy Standard",
          fr = "le pourcentage de ménages vivant dans des logements ne disposant pas d'un nombre suffisant de chambres à coucher selon la norme d'occupation nationale") %>% 
  add_row(en = "the percentage of households living in dwellings requiring major repairs",
          fr = "le pourcentage de ménages vivant dans des logements nécessitant des réparations importantes") %>% 
  add_row(en = "the percentage of owner households that spend more than 30% of their income on shelter costs",
          fr = "le pourcentage de ménages propriétaires qui consacrent plus de 30 % de leur revenu aux frais de logement") %>% 
  add_row(en = "the percentage of renter households that spend more than 30% of their income on shelter costs",
          fr = "le pourcentage de ménages locataires qui consacrent plus de 30 % de leur revenu aux frais de logement") %>% 
  add_row(en = "the percentage of households that have moved in the past year",
          fr = "le pourcentage de ménages qui ont déménagé au cours de l'année écoulée") %>% 
  add_row(en = "the percentage of households that have moved in the past five years",
          fr = "le pourcentage de ménages qui ont déménagé au cours des cinq dernières années") %>% 
  add_row(en = "median before-tax household income",
          fr = "revenu médian des ménages avant impôt") %>% 
  add_row(en = "the percentage of households with an income less then $50,000",
          fr = "le pourcentage de ménages dont le revenu est inférieur à 50 000 $.") %>% 
  add_row(en = "the percentage of households with an income between $50,000 and $100,000",
          fr = "le pourcentage de ménages dont le revenu est compris entre 50 000 et 100 000 dollars") %>% 
  add_row(en = "the percentage of households with an income higher than $100,000",
          fr = "le pourcentage de ménages dont le revenu est supérieur à 100 000 dollars") %>% 
  add_row(en = "the prevalence of low income in private households based on the Low income measure, after-tax(LIM-AT)",
          fr = "la prévalence du faible revenu dans les ménages privés, basée sur la mesure du faible revenu, après impôt (MFR-ApI)") %>% 
  add_row(en = "the percentage of residents who are foreign-born",
          fr = "le pourcentage de résidents nés à l'étranger") %>% 
  add_row(en = "the percentage of people who have immigrated in the last five years",
          fr = "le pourcentage de personnes qui ont immigré au cours des cinq dernières années") %>% 
  add_row(en = "the percentage of people who identify as part of one or more visible minority groups",
          fr = "le pourcentage de personnes qui s'identifient comme faisant partie d'un ou plusieurs groupes de minorités visibles") %>% 
  add_row(en = "the percentage of people who are of aboriginal identity",
          fr = "le pourcentage de personnes qui ont une identité autochtone") %>% 
  add_row(en = "the percentage of people who drive a privately owned car or truck to work",
          fr = "le pourcentage de personnes qui conduisent une voiture ou un camion privé pour se rendre au travail") %>% 
  add_row(en = "the percentage of people who walk or cycle to work",
          fr = "le pourcentage de personnes qui se rendent au travail à pied ou à vélo") %>% 
  add_row(en = "the percentage of people who use public transit to get to work",
          fr = "le pourcentage de personnes qui utilisent les transports en commun pour se rendre au travail") %>% 
  add_row(en = "the percentage of people whose commute time is less than 15 minutes",
          fr = "le pourcentage de personnes dont le temps de trajet est inférieur à 15 minutes") %>% 
  add_row(en = "the percentage of people whose commute time is between 15 and 45 minutes",
          fr = "le pourcentage de personnes dont le temps de trajet est compris entre 15 et 45 minutes") %>% 
  add_row(en = "the percentage of people whose commute time is longer than 45 minutes",
          fr = "le pourcentage de personnes dont le temps de trajet est supérieur à 45 minutes") %>% 
  add_row(en = "the percentage of the workforce in professional and managerial occupations, based on the North American Industry Classification System",
          fr = "le pourcentage de la main-d'œuvre dans les professions libérales et les postes de gestion, selon le Système de classification des industries de l'Amérique du Nord.") %>% 
  add_row(en = "the percentage of the workforce in artistic and cultural occupations, based on the North American Industry Classification System",
          fr = "le pourcentage de la main-d'œuvre dans les professions artistiques et culturelles, selon le Système de classification des industries de l'Amérique du Nord.") %>% 
  add_row(en = "the percentage of census families with children out of total households",
          fr = "le pourcentage de familles de recensement avec enfants par rapport au total des ménages") %>% 
  add_row(en = "the percentage of one person households out of total households",
          fr = "le pourcentage de ménages d'une personne par rapport au total des ménages") %>% 
  add_row(en = "the percentage of individuals that only know French as an official language",
          fr = "le pourcentage d'individus qui ne connaissent que le français comme langue officielle") %>% 
  add_row(en = "the percentage of individuals that only know English as an official language",
          fr = "le pourcentage d'individus qui ne connaissent que l'anglais comme langue officielle") %>% 
  add_row(en = "the percentage of individuals that know both official languages (French and English)",
          fr = "le pourcentage de personnes qui connaissent les deux langues officielles (français et anglais)") %>% 
  add_row(en = "the percentage of individuals that do not know either of the official languages (French or English)",
          fr = "le pourcentage d'individus qui ne connaissent aucune des deux langues officielles (français ou anglais)") %>% 
  add_row(en = "the percentage of the population aged between 0 and 14 years old",
          fr = "le pourcentage de la population âgée de 0 à 14 ans") %>% 
  add_row(en = "the percentage of the population aged between 15 and 64 years old",
          fr = "le pourcentage de la population âgée de 15 à 64 ans") %>% 
  add_row(en = "the percentage of the population aged 65 and above",
          fr = "le pourcentage de la population âgée de 65 ans et plus") %>% 
  add_row(en = "the percentage of the population aged 15 and over holding a degree at bachelor level or above",
          fr = "le pourcentage de la population âgée de 15 ans et plus détenant un diplôme de niveau licence ou plus") %>% 
  add_row(en = "the percentage of the population aged 15 and over with no certificate, diploma or degree",
          fr = "le pourcentage de la population âgée de 15 ans et plus ne possédant aucun certificat, diplôme ou grade") %>% 
  add_row(en = "the potential for active living",
          fr = "le potentiel de la vie active") %>% 

  # Description
  add_row(en = paste0("The whole of an ecosystem is more than the sum of its parts. ",
                      "The health and resilience of our urban green spaces are determined ",
                      "by the quantity, quality, and composition of the species with cohabitat with."), 
          fr = paste0("L'ensemble d'un écosystème est plus que la somme de ses parties. ", 
                      "La santé et la résilience de nos espaces verts urbains sont déterminées ", 
                      "par la quantité, la qualité et la composition des espèces avec lesquelles nous cohabitons.")) %>%
  add_row(en = paste0("Montreal's biodiversity is the result of many competing factors..."), 
          fr = paste0("La biodiversité de Montréal est le résultat de nombreux facteurs concurrents...")) %>%
  add_row(en = "Hello Shiny!", fr = "French") %>%
  
  # Website descriptions
  add_row(en = paste0("Dashboards offer a tool for communicating sustainability data in a visually ", 
                      "based digital platform. We see a gap in current dashboards going beyond the ", 
                      "visualization of pre-existing data at static scales, leaving room for a more ", 
                      "future-oriented, scalable, and interactive model."), 
          fr = paste0("Les tableaux de bord offrent un outil pour communiquer des données sur ", 
                      "le développment durable dans une plate-forme numérique visuelle. Nous constatons ", 
                      "une lacune dans les tableaux de bord actuels allant au-delà de la visualisation ", 
                      "de données préexistantes à des échelles statiques, laissant la place à un modèle ", 
                      "plus orienté vers l'avenir, modulable et interactif.")) %>%
  add_row(en = paste0("Existing data-driven approaches to urban sustainability are characterized by static ", 
                      "data, limited user interaction, and the oversimplification of complex urban issues. ", 
                      "They provide little opportunity for user engagement and exploration of questions ", 
                      "connecting different data and issues."), 
          fr = paste0("Les approches actuelles fondées sur les données étudiant la durabilité urbaine ", 
                      "se caractérisent par des données statiques, une interaction limitée pour les utilisateurs ",  
                      "et une simplification excessive de problèmes urbains complexes. Elles offrent peu ", 
                      "d’opportunités pour l’engagement des utilisateurs et l’exploration de questions reliant ", 
                      "différentes données et problèmes.")) %>%
  add_row(en = paste0("Some of the limitations of existing dashboards include a bias towards quantifiable, ", 
                      "measurable components of sustainability, and a reliance on data with potential bias. ", 
                      "Furthermore, they often attempt to play the role of a neutral force to communicate ", 
                      "“objective” information on cities."), 
          fr = paste0("Parmi les limites des tableaux de bord existants, on peut citer la préférence ", 
                      "accordée aux éléments quantifiables et mesurables de la durabilité, et la dépendance ", 
                      "à l'égard de données potentiellement biaisées. En outre, ils tentent souvent de jouer ", 
                      "le rôle d'une force neutre pour communiquer des informations “objectives” sur les villes.")) %>%
  add_row(en = paste0("Sustainability dashboards should build upon best practices to provide useful tools for ", 
                      "individuals and cities alike to examine the many facets of urban sustainability and ", 
                      "question existing assumptions."), 
          fr = paste0("Les tableaux de bord sur la durabilité devraient s'appuyer sur les meilleures pratiques ", 
                      "afin de fournir des outils utiles aux individus comme aux villes pour examiner les ",
                      "nombreuses facettes de la durabilité urbaine et remettre en question les hypothèses ", 
                      "existantes.")) %>%
  add_row(en = paste0("Maintaining transparency with data and methodologies, ensuring public participation ",
                      "and accurate representation of underprivileged communities, and using engaging and ", 
                      "accessible tools contribute to the success of a dashboard."), 
          fr = paste0("Maintenir la transparence des données et des méthodologies, s'assurer de la participation ", 
                      "du public et de la représentation exacte des communautés défavorisées, ainsi qu'utiliser ", 
                      "des outils attrayants et accessibles contribuent au succès d'un tableau de bord.")) %>%
  add_row(en = paste0("Sus aims to more accurately represent and better engage urban residents in order to ", 
                      "harness the momentum surrounding technologically-based approaches to sustainability ", 
                      "for public good."), 
          fr = paste0("Sus vise à représenter plus justement et à mieux impliquer les résidents urbains ", 
                      "afin d'exploiter le momentum entourant les approches technologiques de la durabilité ", 
                      "pour le bien public.")) %>%
  add_row(en = "Further resources:", fr = "Ressources additionnelles") %>%
  
  # Sidebar menu
  add_row(en = "Active living potential", fr = "Potentiel de vie active") %>%
  add_row(en = "Commuter mode switching", fr = "Changement de mode de transport") %>%
  add_row(en = "Pedestrian realm", fr = "La voie piétonne") %>%
  
  # General info
  add_row(en = "Why a dashboard?", fr = "Pourquoi un tableau de bord?") %>%
  add_row(en = "Meet the team", fr = "Rencontrez l'équipe") %>%
  add_row(en = "Nature-based solutions", fr = "Solutions basées sur la nature") %>%
  add_row(en = "Simulation", fr = "Simulation") %>%
  
  # Built environment
  add_row(en = "Built environment", fr = "Le cadre bâti") %>%
  add_row(en = "Pedestrian capacity for social distancing", 
          fr = "Capacité de distanciation sociale dans la voie piétonne") %>%
  add_row(en = "Pedestrian capacity for social distancing (census tracts)", 
          fr = "Capacité de distanciation sociale dans la voie piétonne (secteur de recensement)") %>%
  add_row(en = "Pedestrian capacity for social distancing (dissemination areas)", 
          fr = "Capacité de distanciation sociale dans la voie piétonne (aire de diffusion)") %>%
  add_row(en = "Explore sidewalks and parks", fr = "Explorez les trottoirs et les parcs") %>%
  add_row(en = paste0("<ul><li>  The top 3 boroughs that have the highest proportion of people living ", 
                      "in census tracts with a low capacity for pedestrian physical distancing were 1) ", 
                      "Le Plateau-Mont-Royal (74%), 2) Villeray-Saint-Michel-Parc-Extension (65%) and 3) ", 
                      "Montréal Nord (60%).</ul>"), 
          fr = paste0("<ul><li>  Les trois arrondissements ayant la plus grande proportion de personnes vivant ", 
                      "dans des secteurs de recensement avec une faible capacité pour une distanciation physique ", 
                      "piétonne sont 1) Le Plateau-Mont-Royal (74%), 2) Villeray-Saint-Michel-Parc-Extension (65%) ", 
                      "and 3) Montréal Nord (60%).</ul>")) %>%
  
  # Bivariate analysis
  add_row(en = "Select your second variable", fr = "Choisissez votre deuxième variable") %>%
  add_row(en = "Net Median Income", fr = "Revenu médian net") %>%
  add_row(en = "Visible Minority Population Proportion", fr = "Minorité visible (proportion de la population)") %>%
  add_row(en = "Immigrant Population Proportion", fr = "Immigrant (proportion de la population)") %>%
  add_row(en = "Montreal Covid-19 Expanded Active Transit Corridors", 
          fr = "Voies actives et sécuritaires de la Ville de Montréal (Covid-19)") %>%
  add_row(en = "Play with the slider to filter the map", 
          fr = "Jouez avec le curseur afin de filtrer et modifier la carte") %>%
  add_row(en = "Explore", fr = "Explorez") %>%
  add_row(en = "Did you know?", fr = "Saviez-vous?") %>%
  
  # names of list
  add_row(en = "Housing", fr = "Logement") %>% 
  add_row(en = "Income", fr = "Revenu") %>% 
  add_row(en = "Immigration", fr = "Immigration") %>% 
  add_row(en = "Transportation", fr = "Déplacement domicile-travail") %>% 
  
  # geographic boundaries
  add_row(en = "Borough", fr = "Arrondissement") %>% 
  add_row(en = "City", fr = "Ville") %>% 
  add_row(en = "borough/city", fr = "de l'arrondissement/de la ville") %>% 
  add_row(en = "boroughs or cities", fr = "arrondissements ou villes") %>% 
  add_row(en = "census tract", fr = "du secteur de recensement") %>%
  add_row(en = "census tracts", fr = "secteurs de recensement") %>% 
  add_row(en = "dissemination area", fr = "de l'aire de diffusion") %>%
  add_row(en = "dissemination areas", fr = "aires de diffusion") %>%
  add_row(en = paste0("Census tract {dat$name}"), 
          fr = paste0("Secteur de recensement {dat$name}")) %>% 
  add_row(en = paste0("Dissemination area {dat$name}"), 
          fr = paste0("Aire de diffusion {dat$name}")) %>% 
  add_row(en = paste0("{dat$name_2} of {place_name}"), 
          fr = paste0("{dat$name_2} de {place_name}")) %>% 
  
  # Housing
  add_row(en = "Tenant-occupied (%)", fr = "Locataire occupant (%)") %>% 
  add_row(en = "Average rent", fr = "Frais de logement mensuels moyens") %>% 
  add_row(en = "Average property value", fr = "Value moyenne des logements") %>% 
  add_row(en = "Unaffordable housing (%)", fr = "Logement inabordable (%)") %>% 
  add_row(en = "Unsuitable housing (%)", fr = "Logement inadéquat (%)") %>% 
  add_row(en = "Housing requiring major repairs (%)", fr = "Logement nécessitant des réparations majeures (%)") %>% 
  add_row(en = "Owner housing stress (%)", fr = "Propriétaires dépensant plus de 30% du revenu sur le logement (%)") %>% 
  add_row(en = "Renter housing stress (%)", fr = "Locataires dépensant plus de 30% du revenu sur le logement (%)") %>% 
  
  # Income
  add_row(en = "Median household income", fr = "Revenu médian des ménages") %>% 
  add_row(en = "Income under $50k (%)", fr = "Revenu inférieur à 50k (%)") %>% 
  add_row(en = "Income between $50k-$100k (%)", fr = "Revenu entre 50k-100k (%)") %>% 
  add_row(en = "Income above $100k (%)", fr = "Revenu supérieur à 100k (%)") %>% 
  add_row(en = "Prevalence of low income (after-tax) (%)", fr = "Prévalence de faible revenu (après taxe) (%)") %>% 
  
  # Immigration
  add_row(en = "Immigration and ethnicity", fr = "Immigration et ethnie") %>% 
  add_row(en = "Immigrants (%)", fr = "Immigrants (%)") %>% 
  add_row(en = "New immigrants (%)", fr = "Nouveaux immigrants (%)") %>% 
  add_row(en = "Visible minorities (%)", fr = "Minorités visibles (%)") %>% 
  
  # Transportation / Trajet domicile-travail
  add_row(en = "Drive to work (%)", fr = "Conducteur (%)") %>% 
  add_row(en = "Walk or cycle to work (%)", fr = "À pied ou à vélo (%)") %>% 
  add_row(en = "Public transit to work (%)", fr = "Transport en commun (%)") %>% 
  add_row(en = "15 minutes to work (%)", fr = "Trajet de 15 minutes (%)") %>% 
  add_row(en = "15-30 minutes to work (%)", fr = "Trajet de 15-30 minutes (%)") %>% 
  add_row(en = "30-45 minutes to work (%)", fr = "Trajet de 30-45 minutes (%)") %>% 
  add_row(en = "45-60 minutes to work (%)" , fr = "Trajet de 45-60 minutes (%)") %>% 
  
  # For my life to be easier
  add_row(en = "----" , fr = "----") %>% 
  add_row(en = " " , fr = " ") %>% 
  
  # Quintiles and quantitative terms
  add_row(en = "much larger than" , fr = "beaucoup plus grand que") %>%
  add_row(en = "larger than" , fr = "plus grand que") %>%
  add_row(en = "almost the same as" , fr = "presque le même que") %>%
  add_row(en = "smaller than" , fr = "plus petit que") %>%
  add_row(en = "much smaller than" , fr = "beaucoup plus petit que") %>%
  add_row(en = "larger" , fr = "plus grand") %>%
  add_row(en = "smaller" , fr = "plus petit") %>%
  add_row(en = "strong" , fr = "fort") %>%
  add_row(en = "poor" , fr = "faible") %>%
  add_row(en = "moderate" , fr = "modéré") %>%
  
  # Correlation
  add_row(en = "positive" , fr = "positif") %>%
  add_row(en = "negative" , fr = "négatif") %>%
  add_row(en = "weak" , fr = "faible") %>%
  add_row(en = "higher" , fr = "plus grandes") %>%
  add_row(en = "lower" , fr = "plus petites") %>%
  add_row(en = "high" , fr = "haut") %>%
  add_row(en = "low" , fr = "bas") %>%
  add_row(en = "similar" , fr = "similaires") %>%
  add_row(en = "with only a few exceptions" , fr = "à quelques exceptions près") %>%
  add_row(en = "although with some exceptions" , fr = "bien qu'avec des exceptions") %>%
  add_row(en = "although with many exceptions" , fr = "bien qu'avec beaucoup d'exceptions") %>%
  
  # Bivariate comparison 
  add_row(en = "dramatically different" , fr = "radicalement différents") %>%
  add_row(en = "substantially different" , fr = "sensiblement différents") %>%
  add_row(en = "considerably different" , fr = "modérément différents") %>%
  
  # CanAle module
  # Chosen value and comparison
  add_row(en = paste0("At the {scale_singular} scale, the {title} varies from ",
                      "{min_val} to {max_val}, with an average value of {mean_val} ",
                      "and a median value of {median_val}. ",
                      "Two thirds of {scale_plural} have a score between {quant_low} ",
                      "and {quant_high}."),
          fr = paste0("À l'échelle {scale_singular}, {title} varie de ",
                      "{min_val} à {max_val}, avec une valeur moyenne de {mean_val} ",
                      "et une valeur médianne de {median_val}. ",
                      "Deux tiers {scale_plural} ont un score se situant entre {quant_low} ",
                      "et {quant_high}.")) %>% 
  
  # Mohawk Territory
  add_row(en = paste0("<strong>Kahnawake Mohawk Territory</strong>",
                      "<p>Statistics Canada does not gather the same ",
                      "data for indigenous reserves in the Census as it does ",
                      "for other jurisdictions, so we cannot display findings ",
                      "here."),
          fr = paste0("<strong>Kahnawake (Réserve indienne)</strong>",
                      "<p>Dans le cadre du recensement, Statistique Canada ne ",
                      "recueille pas les mêmes données pour les réserves",
                      "autochtones que dans les autres juridictions, nous ne ",
                      "pouvons donc pas afficher de résultats ici.")) %>%
  
  # CanALE active living potential
  add_row(en = "CanALE index", fr = "Indice AVA-Can") %>% 
  add_row(en = "View in 3D", fr = "Voir en 3D") %>% 
  add_row(en = "Active living potential: the CanALE index",
          fr = "Potentiel de vie active: l'indice AVA-Can") %>% 
  add_row(en = paste0("<strong>{place_heading}</strong>", 
                      
                      "<p>{place_name} has a population of ",
                      "{prettyNum(dat$population, ',')} and a {title} ",
                      "score of {round(poly_value, 2)}, which is {larger_smaller} ",
                      "the region-wide median of {median_val}.", 
                      
                      "<p>{place_name} has {poor_strong} potential for active ", 
                      "living, with a CanALE index score higher than {percentile}% ",
                      "of {scale_plural} in the Montreal region."),
          fr = paste0("<strong>{place_heading}</strong>", 
                      
                      "<p>{place_name} a une population de ",
                      "{prettyNum(dat$population, ',')} et un score {title} ",
                      "de {round(poly_value, 2)}, ce qui est {larger_smaller} ",
                      "la médiane régionale de {median_val}.", 
                      
                      "<p>{place_name} a un potentiel {poor_strong} de vie ", 
                      "active, avec un score d'indice AVA-Can plus grand que {percentile}% ",
                      "des {scale_plural} dans la région de Montréal.")) %>% 
  
  # Correlation explanation
  add_row(en = paste0("<p>{var_explanation}", 
                      "<p>The {title} has effectively no correlation ",
                      "({correlation}) with {var_name} at the ",
                      "{scale_singular} scale.",
                      "<p>This means that, at the {scale_singular} scale, ", 
                      "there is no relationship between the two variables."),
          fr = paste0("<p>{var_explanation}", 
                      "<p>{title} n'a en fait aucune corrélation ",
                      "({correlation}) avec la variable '{var_name}' à l'échelle ",
                      "'{scale_singular}'.",
                      "<p>Cela signifie que, à l'échelle '{scale_singular}', ", 
                      "il n'y a pas de relation entre ces deux variables.")) %>% 
  add_row(en = paste0("<p>{var_explanation}", 
                      "<p>The {title} has a {strong_weak} {pos_neg} ",
                      "correlation ({correlation}) with '{tolower(var_name)}' at the ",
                      "{scale_singular} scale.",
                      "<p>This means that, in general, {scale_plural} with higher ",
                      "potential for active living tend to have {higher_lower} ",
                      "values for '{tolower(var_name)}', {high_low_disclaimer}."),
          fr = paste0("<p>{var_explanation}", 
                      "<p>{title} a un indice de corrélation {strong_weak} et {pos_neg} ",
                      "({correlation}) avec la variable '{tolower(var_name)}' à l'échelle ",
                      "'{scale_singular}'.",
                      "<p>Cela signifie qu'en général, les {scale_plural} avec un haut ",
                      "potentiel de vie active tendent à avoir des {higher_lower} ",
                      "valeurs pour la variable '{tolower(var_name)}', {high_low_disclaimer}.")) %>% 
  
  # Bivariate comparison
  add_row(en = paste0("<strong>{place_heading}</strong>", 
                      
                      "<p>{place_name} has a population of ",
                      "{prettyNum(dat$population, ',')}, a {title} score ",
                      "of {round(poly_value_1, 2)}, and a '{tolower(var_name)}' ",
                      "value of {round(poly_value_2, 2)}. ",
                      
                      "<p>These two scores are {relative_position}, in relative ",
                      "terms. {place_name} has a {title} score higher ",
                      "than {percentile_left}% of {scale_plural} and ",
                      "a '{tolower(var_name)}' score higher than ", 
                      "{percentile_right}% of {scale_plural} in the ",
                      "Montreal region."),
          fr = paste0("<strong>{place_heading}</strong>", 
                      
                      "<p>{place_name} a une population de ",
                      "{prettyNum(dat$population, ',')}, un score {title} ",
                      "de {round(poly_value_1, 2)}, et une valeur pour la variable '{tolower(var_name)}' ",
                      "de {round(poly_value_2, 2)}. ",
                      
                      "<p>Ces deux scores sont {relative_position}, en termes ",
                      "relatifs. {place_name} a un score {title} plus grand ",
                      "que {percentile_left}% des {scale_plural} et ",
                      "un score pour la variable '{tolower(var_name)}' plus grand que ", 
                      "{percentile_right}% des {scale_plural} dans la ",
                      "région de Montréal.")) %>% 
  
  # Did you know
  add_row(en = "Hide" , fr = "En voir moins") %>%
  add_row(en = "Learn more" , fr = "En savoir plus") %>%
  add_row(en = "Show" , fr = "Afficher") %>%
  
  # Pedestrian realm 
  add_row(en = "Perform a Bivariate Analysis" , fr = "Effectuez une analyse bivariée") %>%
  add_row(en = "Walkable Access to Key Amenities" , fr = "Accès à pied aux services de base") %>%
  add_row(en = "Net Median Income" , fr = "Revenu médian net") %>%
  add_row(en = "Visible Minority Population" , fr = "Minorité visible (population)") %>%
  add_row(en = "Immigrant Population" , fr = "Population d'immigrants") %>%
  add_row(en = "Original Plan (May 15, 2020)" , fr = "Plan initial (15 mai 2020)") %>%
  add_row(en = "Revised Plan (July 25, 2020)" , fr = "Plan révisé (25 juillet 2020)") %>%
  add_row(en = "Choose more variables and explore further" , fr = "Choisissez d'autres variables et explorez davantage") %>%
  add_row(en = "Population density per square km" , fr = "Densité de population par kilomètre carré") %>%
  add_row(en = "Pedestrian social distancing capacity" , fr = "Capacité de distanciation sociale des piétons") %>%
  add_row(en = "Work commutes by car (%)" , fr = "Trajets domicile-travail en voiture (%)") %>%
  add_row(en = "Trajet MTL 2016 data on pedestrian flows" , fr = "Données Trajet MTL 2016 sur les déplacements des piétons") %>%
  add_row(en = "Explore" , fr = "Explorez") %>%  
  add_row(en = "Capacity for pedestrian social distancing" , fr = "Capacité de distanciation sociale des piétons") %>% 
  add_row(en = "Capacity for pedestrian social distancing (%)" , fr = "Capacité de distanciation sociale des piétons (%)") %>% 
  add_row(en = paste0("Capacity of local population to make ",
                      "trips on foot while maintaining 2 meters distance (%)"),
          fr = paste0("Capacité de la population locale à effectuer des déplacements à ",
                      "pied tout en conservant une distance de 2 mètres (%)")) %>%  
  add_row(en = "Log of Population density / km2" , fr = "Log de la densité de population / km2") %>% 
  add_row(en = "Pedestrian trips per sqm of walkable space index (0 = average)" , fr = "Indice des déplacements à pied par m² d'espace piétonnier (0 = moyenne)") %>%
  add_row(en = "Clear selection" , fr = "Effacez sélection") %>% 
  
  # Pedestrian realm, social distancing capacity
  add_row(en = paste0("At the census tract scale, after removing outliers with a ",
                      "population below 500, the capacity for pedestrian social distancing varies from ",
                      "{min_ped_ct}% to {max_ped_ct}%, with an average value of {mean_ped_ct}% ",
                      "and a median value of {median_ped_ct}%. ",
                      "Two thirds of census tracts have a score between {quant_low_ped_ct}% ",
                      "and {quant_high_ped_ct}%. Out of the 532 census tracts, ",
                      "227 of them have a capacity score below 100%, ",
                      "while 85 of them have a capacity score below 50%."),
          fr = paste0("À l'échelle du secteur de recensement, après avoir éliminé les données aberrantes avec une ", 
                      "population en dessous de 500 personnes, la capacité pour la distanciation sociale des piétons varie de ",
                      "{min_ped_ct}% à {max_ped_ct}%, avec une valeur moyenne de {mean_ped_ct} ",
                      "et une valeur médiane de {median_ped_ct}%. ",
                      "Deux tiers des secteurs de recensement ont un score entre {quant_low_ped_ct}% ",
                      "et {quant_high_ped_ct}%. Sur les 532 secteurs de recensement, ",
                      "227 d'entre eux ont un score de capacité en dessous de 100%, ",
                      "alors que 85 d'entre eux ont un score de capacité en dessous de 50%.")) %>% 
  add_row(en = paste0("At the dissemination area scale, after removing outliers with a population below 100, the capacity for pedestrian social distancing varies from ",
                      "{min_da_uni}% to {max_da_uni}%, with an average value of {mean_da_uni}% ",
                      "and a median value of {median_da_uni}%. ",
                      "Two thirds of dissemination areas have a score between {quant_low_da_uni}% ",
                      "and {quant_high_da_uni}%."),
          fr = paste0("À l'échelle de l'aire de diffusion, après avoir éliminé les valeurs aberrantes avec une ", 
                      "population en dessous de 100 personnes, la capacité pour la distanciation sociale des piétons varie de ",
                      "{min_da_uni}% à {max_da_uni}%, avec une valeur moyenne de {mean_da_uni}% ",
                      "et une valeur médiane de {median_da_uni}%. ",
                      "Deux tiers des aires de diffusion ont un score entre {quant_low_da_uni}% ",
                      "et {quant_high_da_uni}%.")) %>% 
  add_row(en = paste0("The dissemination area {dat_ped_uni$ID} has a population of ",
                      "{prettyNum(dat_ped_uni$population, ',')} and a pedestrian social distancing capacity ",
                      "of {round(poly_value_ped_uni, 2)}%, which is {larger_smaller_ped_uni} ",
                      "the region-wide median of {median_da_uni}%.", 
                      
                      "<p>Dissemination area {dat_ped_uni$ID} offers a {poor_strong_ped_uni} capacity for its residents to practice social distancing in the local pedestrian realm."),
          fr = paste0("L'aire de diffusion {dat_ped_uni$ID} a une population de ",
                      "{prettyNum(dat_ped_uni$population, ',')} et une capacité de distanciation sociale des piétons ",
                      "de {round(poly_value_ped_uni, 2)}%, ce qui est {larger_smaller_ped_uni} ",
                      "que la médiane régionale de {median_da_uni}%.", 
                      
                      "<p>L'aire de diffusion {dat_ped_uni$ID} offre un potentiel {poor_strong_ped_uni} pour ses résidents de pratiquer la distanciation sociale dans les voies piétonnes locales.")) %>%
  
  # Pedestrian realm, correlation with other variables
  add_row(en = paste0("The capacity for pedestrian social distancing metric has effectively no correlation ",
                      "({correlation_ped}) with {var_name_ped} at the dissemination area scale. ",
                      "<p>This means that, at the dissemination area scale, ",
                      "there is no relationship between the two variables."),
          fr = paste0("La mesure de la capacité pour la distanciation sociale des piétons n'a en fait aucune corrélation ",
                      "({correlation_ped}) avec '{var_name_ped}' à l'échelle de l'aire de diffusion. ",
                      "<p>Cela signifie que, à l'échelle de l'aire de diffusion, ",
                      "il n'y a pas de relation entre ces deux variables.")) %>% 
  add_row(en = paste0("The capacity for pedestrian social distancing metric has a {strong_weak_ped} {pos_neg_ped} ",
                      "correlation ({correlation_ped}) with '{tolower(var_name_ped)}' at the dissemination area scale. ",
                      "<p>This means that, in general, dissemination areas with higher ",
                      "capacities to allow for pedestrian social distancing tend to have {higher_lower_ped} ",
                      "'{tolower(var_name_ped)}' values, {high_low_disclaimer_ped}."),
          fr = paste0("La mesure de la capacité de distanciation sociale des pétions a un coefficient de corrélation {strong_weak_ped} {pos_neg_ped} ",
                      "({correlation_ped}) avec la variable '{tolower(var_name_ped)}', à l'échelle de l'aire de diffusion. ",
                      "<p>Cela signifie que, en général, les aires de diffusion avec de plus grandes ",
                      "capacités de distanciation sociale des piétons tendent à avoir de {higher_lower_ped} ",
                      "valeurs pour la variable '{tolower(var_name_ped)}', {high_low_disclaimer_ped}.")) %>% 
  add_row(en = paste0("Dissemination area {dat_ped_biv$ID} has a population of ",
                      "{prettyNum(dat_ped_biv$population, ',')}, a capacity for pedestrian social distancing ",
                      "of {round(poly_value_1, 2)}%, and a '{tolower(var_name_ped)}' ",
                      "value of {round(poly_value_2, 2)}. ",
                      
                      "<p>These two scores are {relative_position}, in relative ",
                      "terms. Dissemination area {dat_ped_biv$ID} has a capacity for pedestrian social distancing higher ",
                      "than {percentile_left}% of dissemination areas and ",
                      "a '{tolower(var_name_ped)}' score higher than ", 
                      "{percentile_right}% of dissemination areas in the ",
                      "Montreal region."),
          fr = paste0("L'aire de diffusion {dat_ped_biv$ID} a une population de ",
                      "{prettyNum(dat_ped_biv$population, ',')}, une capacité de distanciation sociale des piétons ",
                      "de {round(poly_value_1, 2)}%, et une valeur pour la variable '{tolower(var_name_ped)}' ",
                      "de{round(poly_value_2, 2)}. ",
                      
                      "<p>Ces deux scores sont {relative_position}, en termes ",
                      "relatifs. L'aire de diffusion {dat_ped_biv$ID} a une capacité de distanciation sociale des piétons plus grande ",
                      "que {percentile_left}% des aires de diffusion et ",
                      "une valeur pour la variable '{tolower(var_name_ped)}' plus grand que ", 
                      "{percentile_right}% des aires de diffusion de la région ",
                      "de Montréal.")) %>% 
  
  # Pedestrian realm, sidewalk width
  add_row(en = "Sidewalk width (meters)" , fr = "Largeur des trottoirs (mètres)") %>% 
  add_row(en = paste0("Sidewalk width in Montreal varies from ",
                      "{min_sidewalk} meters to {max_sidewalk} meters, ",
                      "with an average value of {mean_sidewalk} meters ",
                      "and a median value of {median_sidewalk} meters. ",
                      "Two thirds of Montreal's sidewalks have widths ",
                      "between {quant_low_sidewalk} meters and {quant_high_sidewalk} meters."),
          fr = paste0("La largeur des trottoirs de Montréal varie de ",
                      "{min_sidewalk} mètres à {max_sidewalk} mètres, ",
                      "avec une valeur moyenne de {mean_sidewalk} mètres ",
                      "et une valeur médiane de {median_sidewalk} mètres. ",
                      "Deux tiers des trottoirs montréalais ont une largeur ",
                      "entre {quant_low_sidewalk} mètres et {quant_high_sidewalk} mètres.")) %>% 
  
  # Mode shift module
  add_row(en = "Shifting car trips to cycling" , fr = "Transférer les trajets de voiture en vélo") %>% 
  add_row(en = "Share of trips taken by car" , fr = "Part des trajets effectués en voiture") %>% 
  add_row(en = "Average commuting distance" , fr = "Distance moyenne du trajet domicile-travail") %>% 
  add_row(en = "Access to cycling infrastructure" , fr = "Accès aux infrastructures cyclables") %>% 
  add_row(en = "% of trips taken by car, by census tract" , fr = "% des trajets effectués en voiture, par secteur de recensement") %>% 
  add_row(en = "Modal shift scenarios" , fr = "Scénarios de transfert modal") %>% 
  add_row(en = "Baseline" , fr = "Base de référence") %>% 
  add_row(en = "Distance" , fr = "Distance") %>% 
  add_row(en = "Elevation/time" , fr = "Élévation/temps") %>% 
  add_row(en = "Show baseline" , fr = "Afficher la base de référence") %>% 
  add_row(en = "Cycling distance (km):" , fr = "Distance parcourue à vélo (km):") %>% 
  add_row(en = "Elevation gain (m):" , fr = "Gain d'élévation (m):") %>% 
  add_row(en = "Time ratio:" , fr = "Ratio de temps:") %>% 
  add_row(en = "VKT Reduction" , fr = "Réduction de KPV") %>% 
  add_row(en = "Cycling network" , fr = "Réseau cycliste") %>% 
  
  # Mode shift, scenarios
  add_row(en = "Criteria: Cycling Distance (km)" , fr = "Critère: Distance parcourue à vélo (km)") %>% 
  add_row(en = "Potential Cyclable Trips (per day)" , fr = "Trajets cyclistes potentiels (par jour)") %>% 
  add_row(en = "VKT Savings (per day)" , fr = "Économies de KPV (par jour)") %>% 
  add_row(en = "Criteria: Elevation Gain (m)" , fr = "Critère: Gain d'élévation (m)") %>% 
  add_row(en = "Criteria: Time Ratio" , fr = "Critère: Ratio de temps") %>%
  
  # Mode shift, legend
  add_row(en = "Access to Cycling Infrastructure (km/sq.km)" , fr = "Accès aux infrastructures cyclables (km/km2)") %>%
  add_row(en = "Share of Car Trips by Origin (%)" , fr = "Part des trajets en voiture par origine (%)") %>%
  add_row(en = "Average Commuting Distance (km)" , fr = "Distance moyenne du trajet domicile-travail (km)") %>%
  
  # Mode shift, map
  add_row(en = "Access to cycling inf. (km/sq.km)" , fr = "Accès aux infr. cyclables (km/km2)") %>%
  add_row(en = "Share of trips taken by car (%)" , fr = "Part des trajets effectués en voiture (%)") %>%
  add_row(en = "Average commuting distance (km)" , fr = "Distance moyenne du trajet domicile-travail (km)") %>%
  add_row(en = "Cycling infrastructure (km/sq.km) by census tract:" , fr = "Infrastructure cyclable (km/km2) par secteur de recensement:") %>%
  add_row(en = "Length of the average commute (km), by census tract:" , fr = "Distance moyenne du trajet domicile-travail (km), par secteur de recensement :") %>%
  add_row(en = "Access to cycling inf. (km/sq.km)" , fr = "Accès aux infr. cyclables (km/km2)") %>%
  
  # Did you know csv
  add_row(en = paste0("21.3% of people walk or cycle to work in areas in the highest class of active living ",
                      "potential. In areas with the lowest active living potential, only 2.5% do, compared to a ",
                      "regional average of 6%."),
          fr = paste0("21,3 % des personnes se rendent au travail à pied ou à vélo dans des zones où le potentiel ",
                      "de vie active est le plus élevé. Dans les zones où le potentiel de vie active est le plus ",
                      "faible, seuls 2,5 % le font, contre une moyenne régionale de 6 %.")) %>% 
  add_row(en = paste0("Two thirds of new immigrants (67.4%) live in areas with the best access to active living ",
                      "potential (ALE class 5)."),
          fr = paste0("Deux tiers des nouveaux immigrants (67,4 %) vivent dans des zones où le potentiel de vie ",
                      "active est le plus grand (Indice AVA classe 5).")) %>% 
  add_row(en = paste0("Two thirds of new immigrants (67.4%) live in areas with the best access to active living ",
                      "potential (ALE class 5)."),
          fr = paste0("Deux tiers des nouveaux immigrants (67,4 %) vivent dans des zones où le potentiel de vie ",
                      "active est le plus élevé (Indice AVA classe 5).")) %>% 
  add_row(en = paste0("11% of new immigrants live in areas with the worst access to active living potential ",
                      "(ALE class 1)."),
          fr = paste0("11 % des nouveaux immigrants vivent dans des zones où l'accès au potentiel de vie active ",
                      "est le plus faible (Indice AVA classe 1).")) %>% 
  add_row(en = paste0("While, overall, there is a trend towards lower income DAs being in high ALE class, roughly ",
                      "340,000 people are in CanALE class 3 or lower AND have median household income less than $50,000."),
          fr = paste0("Alors que, généralement, on observe une tendance à ce que les AD à faible revenu fassent partie ",
                      "de la classe AVA élevée, environ 340 000 personnes font partie de la classe AVA-Can 3 ou moins et ",
                      "ont un revenu par ménage médian inférieur à 50 000 $.")) %>% 
  add_row(en = paste0("Areas in class 4 of active living potential have the highest average dwelling values, but nearly ",
                      "10% fewer renters than areas with active living class 5."),
          fr = paste0("Les zones de classe 4 de potentiel de vie active ont les valeurs propriétés moyennes les plus ",
                      "élevées, mais comptent près de 10 % de locataires de moins que les zones avec un potentiel de ",
                      "de vie active de classe 5.")) %>% 
  add_row(en = paste0("Roughly 90% of the region’s population live within a 1km walk of a transit station. The vast ",
                      "majority of areas which lack access are located off the island of Montreal."),
          fr = paste0("Environ 90 % de la population de la région vit à moins d'un kilomètre à pied d'une station de ",
                      "transport en commun. La grande majorité des zones qui ne sont pas accessibles sont situées à ",
                      "l'extérieur de l'Île de Montréal.")) %>% 
  add_row(en = paste0("The Plateau-Mont-Royal has the highest active mode share to work (36% walking or biking), ",
                      "followed by Ville Marie with 32.5%."),
          fr = paste0("Le Plateau-Mont-Royal a la part la plus élevée de modes de transport actifs pour travailler (36% à pied ",
                      "ou à vélo), suivi de Ville Marie avec 32,5%.")) %>% 
  add_row(en = paste0("Saint-Isidore, Sainte-Marthe-sur-le-Lac, Pointe-Calumet, Mascouche, Saint-Joseph-du-Lac, and Léry all ",
                      "have active mode shares of less than 2% to work."),
          fr = paste0("Saint-Isidore, Sainte-Marthe-sur-le-Lac, Pointe-Calumet, Mascouche, Saint-Joseph-du-Lac et Léry ont tous ",
                      "des pourcentages de modes de transport actifs inférieurs à 2% pour les déplacements domicile-travail.")) %>% 
  add_row(en = paste0("Pierrefonds-Roxboro and L’Île-Bizard-Sainte-Geneviève are the two Montreal boroughs with the lowest ",
                      "active mode share to work (less than 3% walk or bike)."),
          fr = paste0("Pierrefonds-Roxboro et L'Île-Bizard-Sainte-Geneviève sont les deux arrondissements de Montréal où le ",
                      "pourcentage de modes de transport actifs pour les déplacements domicile-travail est le plus faible ",
                      "(moins de 3 % à pied ou à vélo).")) %>% 
  add_row(en = paste0("Hampstead, Côte-des-Neiges-Notre-Dame-de-Grâce, and Villeray-Saint-Michel-Parc-Extension all have ",
                      "high active living potential (ALE class 4 or higher) and active mode shares of less than 15%."),
          fr = paste0("Hampstead, Côte-des-Neiges-Notre-Dame-de-Grâce et Villeray-Saint-Michel-Parc-Extension ont ",
                      "tous un potentiel de vie active élevé (Indice AVA classe 4 ou plus) et des pourcentages de modes de transport ",
                      "actifs inférieurs à 15 %.")) %>%
  add_row(en = paste0("Sainte-Julie has the highest active mode share (9.1%) of any borough or municipality with low active ",
                      "living potential, followed by Senneville (5.8%) and Montréal-Est (5.8%)."),
          fr = paste0("Sainte-Julie a la plus grande proportion de mode de transport actif (9,1 %) de tous les arrondissements ",
                      "ou municipalités à faible potentiel de vie active, suivie de Senneville (5,8 %) et de Montréal-Est (5,8 %).")) %>%
  add_row(en = paste0("Montréal-Est has the highest proportion (22.2%) of people who take public transit to work of any ",
                      "area with low walkability."),
          fr = paste0("De tous les secteurs où le potentiel piétonnier est faible, c'est à Montréal-Est que l'on trouve ",
                      "la plus forte proportion (22,2 %) de personnes qui prennent le transport en commun pour se rendre ",
                      "au travail.")) %>%
  add_row(en = paste0("Hampstead has the lowest proportion (15.6%) of people who take public transit to work of any area ",
                      "with high walkability, and Côte-des-Neiges-Notre-Dame-de-Grâce has the highest (46.7%)."),
          fr = paste0("Hampstead a la plus faible proportion (15,6 %) de personnes qui prennent le transport en commun ",
                      "pour les déplacements domicile-travail parmi tous les secteurs à fort potentiel piétonnier, et ",
                      "Côte-des-Neiges-Notre-Dame-de-Grâce a la plus forte proportion (46,7 %).")) %>%
  add_row(en = paste0("The proportion of people who drive to work in areas with low walkability ranges from 71.2% to ",
                      "95.7%. Montréal-Est has the lowest proportion with 71.2%."),
          fr = paste0("La proportion de personnes qui se rendent au travail en voiture dans des zones où le potentiel ",
                      "piétonnier est faible varie entre 71,2 % et 95,7 %. C'est à Montréal-Est que la proportion est la plus ",
                      "petite, soit 71,2 %.")) %>%
  add_row(en = paste0("The proportion of people who drive to work in areas with high walkability ranges from 24.9% in ",
                      "Le Plateau-Mont-Royal to 78.9% in Hampstead."),
          fr = paste0("La proportion de personnes qui se rendent au travail en voiture dans des zones à fort potentiel ",
                      "piétonnier varie de 24,9 % pour le Plateau-Mont-Royal à 78,9 % à Hampstead.")) %>%
  add_row(en = paste0("Sainte-Julie has the highest proportion of people who commute less than 15 minutes to work, and ",
                      "has low active living potential (ALE class 1)."),
          fr = paste0("Sainte-Julie compte la plus forte proportion de personnes qui se rendent au travail en moins de ",
                      "15 minutes dans les arrondissements ou municipalités ayant un faible potentiel de vie active ",
                      "(Indice AVA classe 1).")) %>%
  add_row(en = paste0("Villeray-Saint-Michel-Parc-Extension and Rosemont-La Petite-Patrie have the lowest proportions ",
                      "of people who commute less than 15 minutes to work (11.2% and 11.9%, respectively) of areas ",
                      "with high walkability."),
          fr = paste0("Villeray-Saint-Michel-Parc-Extension et Rosemont-La Petite-Patrie affichent les plus petites ",
                      "proportions de personnes qui se rendent au travail en moins de 15 minutes (respectivement ",
                      "11,2 % et 11,9 %) parmi les régions où le potentiel piétonnier est élevé.")) %>%
  add_row(en = paste0("Mont-Royal (medium active living potential) has the highest proportion (49.1%) of commutes ",
                      "to work between 15-30 minutes, while Mercier (low active living potential) has the lowest ",
                      "proportion for the same time (19.7%)."),
          fr = paste0("Le Mont-Royal (potentiel de vie active moyen) présente la plus grande proportion (49,1 %) ",
                      "de trajets domicile-travail de 15 à 30 minutes, tandis que le Mercier (faible potentiel de ",
                      "vie active) présente la plus faible proportion pour la même durée (19,7 %).")) %>%
  add_row(en = paste0("Rosemont-La Petite-Patrie (high active living potential) has the highest proportion (35.1%) ",
                      "of commutes to work between 30-45 minutes, while Sainte-Julie (low active living potential) ",
                      "has the lowest proportion for the same time (9.1%)."),
          fr = paste0("Rosemont-La Petite-Patrie (fort potentiel de vie active) a la plus grande proportion (35,1 %) ",
                      "de trajets domicile-travail de 30 à 45 minutes, tandis que Sainte-Julie (faible potentiel ",
                      "de vie active) a la plus petite proportion pour la même durée (9,1 %).")) %>%
  add_row(en = paste0("Léry (low active living potential) has the highest proportion (19.2%) of commutes to work ",
                      "between of up to 60 minutes, while Westmount (high active living potential) has the lowest ",
                      "proportion for the same time (5.5%)."),
          fr = paste0("Léry (faible potentiel de vie active) présente la plus grande proportion (19,2 %) de trajets ",
                      "domicile-travail d'une durée maximale de 60 minutes, tandis que Westmount (fort potentiel ",
                      "de vie active) présente la plus petite proportion pour la même durée (5,5 %).")) %>%
  add_row(en = paste0("Hampstead has the highest proportion of people with income greater than $100,000 (55.0%) ",
                      "of high walkable boroughs or municipalities, closely followed by Westmount (50.1%)."),
          fr = paste0("Hampstead a la plus grande proportion de personnes ayant un revenu supérieur à 100 000 $ ",
                      "(55,0 %) parmi les arrondissements ou municipalités avec un fort potentiel piétonnier, ",
                      "suivie de près par Westmount (50,1 %).")) %>%
  add_row(en = paste0("Sainte-Anne-de-Bellevue has the highest proportion of people with income greater than ",
                      "$100,000 (55.0%) of less walkable boroughs or municipalities, closely followed by ",
                      "Beaconsfield (59.7%)."),
          fr = paste0("Sainte-Anne-de-Bellevue compte la plus forte proportion de personnes ayant un revenu ",
                      "supérieur à 100 000 $ (55,0 %) parmi les arrondissements ou municipalités avec un faible ",
                      "potentiel piétonnier, suivie de près par Beaconsfield (59,7 %).")) %>%
  add_row(en = paste0("Montréal-Est has the highest proportion of people with income less than $50,000 (49.1%) ",
                      "of all boroughs or municipalities with low active living potential."),
          fr = paste0("Montréal-Est compte la plus grande proportion de personnes ayant un revenu inférieur à ",
                      "50 000 $ (49,1 %) de tous les arrondissements ou municipalités à faible potentiel de ",
                      "vie active.")) %>%
  add_row(en = paste0("Villeray-Saint-Michel-Parc-Extension has the highest proportion of people with income ",
                      "less than $50,000 (57.7%) of all boroughs or municipalities with high active living potential."),
          fr = paste0("Villeray-Saint-Michel-Parc-Extension compte la plus grande proportion de personnes ",
                      "ayant un revenu inférieur à 50 000 $ (57,7 %) de tous les arrondissements ou municipalités ",
                      "à fort potentiel de vie active.")) %>%
  add_row(en = paste0("Sainte-Anne-de-Bellevue has the highest overall median income ($130,553) of any area with ",
                      "low active living potential, and Montréal-Est has the lowest ($50,413)."),
          fr = paste0("Sainte-Anne-de-Bellevue a le revenu médian global le plus élevé (130 553 $) de toutes ",
                      "les régions à faible potentiel de vie active, et Montréal-Est a le plus bas (50 413 $).")) %>%
  add_row(en = paste0("Côte-des-Neiges-Notre-Dame-de-Grâce has the highest proportion of new immigrants (12%) ",
                      "of any borough with high active living potential (average ALE class >4 across the ",
                      "entire borough). It also has the highest proportion of immigrants in general (46.7%) ",
                      "for the same conditions."),
          fr = paste0("Côte-des-Neiges-Notre-Dame-de-Grâce a la plus grande proportion de nouveaux immigrants ",
                      "(12 %) de tous les arrondissements à fort potentiel de vie active (classe AVA moyenne > 4 ",
                      "sur l'ensemble de l'arrondissement). Elle a également la plus forte proportion d'immigrants ",
                      "en général (46,7 %) pour les mêmes conditions.")) %>%
  add_row(en = paste0("Senneville has the highest proportion of new immigrants (4.4%) of any borough or municipality ",
                      "with low active living potential, followed by L’Île-Bizard-Sainte-Geneviève (3.6%)."),
          fr = paste0("Senneville compte la plus forte proportion de nouveaux immigrants (4,4 %) de tous les ",
                      "arrondissements ou municipalités à faible potentiel de vie active, suivie de ",
                      "L'Île-Bizard-Sainte-Geneviève (3,6 %).")) %>%
  add_row(en = paste0("9910 (20%) of immigrants living in areas with low walkability live in Terrebonne. 1040 ",
                      "of those people are new immigrants."),
          fr = paste0("9910 (20 %) des immigrants vivant dans des zones à faible potentiel piétonnier vivent à ",
                      "Terrebonne. 1040 de ces personnes sont de nouveaux immigrants.")) %>%
  add_row(en = paste0("25 percent of immigrants (76,190 people) living in areas with high walkability live in ",
                      "Côte-des-Neiges-Notre-Dame-de-Grâce. 19,605 of those people (25.7%) are new immigrants."),
          fr = paste0("25 % des immigrants (76 190 personnes) vivant dans des secteurs à fort potentiel piétonnier ",
                      "habitent Côte-des-Neiges-Notre-Dame-de-Grâce. 19 605 de ces personnes (25,7 %) sont de nouveaux ",
                      "immigrants.")) %>%
  add_row(en = paste0("302,195 immigrants live in areas with on average high walkability, while 48,635 immigrants ",
                      "live in areas with on average low walkability."),
          fr = paste0("302 195 immigrants vivent dans des zones où le potentiel piétonnier est, en moyenne, élevé tandis ",
                      "que 48 635 immigrés vivent dans des zones où le potentiel piétionnier est, en moyenne, faible.")) %>%
  add_row(en = paste0("Hampstead has the lowest proportion of renters (31.7%) of all areas with high active living ",
                      "potential, and Côte-des-Neiges-Notre-Dame-de-Grâce has the highest proportion (73.7%)."),
          fr = paste0("Hampstead a la plus petite proportion de locataires (31,7 %) de tous les secteurs à fort ",
                      "potentiel de vie active, et Côte-des-Neiges-Notre-Dame-de-Grâce a la plus grande proportion (73,7 %).")) %>%
  add_row(en = paste0("Montréal-Est has the highest proportion of tenants (56.1%) of all areas with low active living ",
                      "potential, followed by Mirabel with 29.3%."),
          fr = paste0("Montréal-Est compte la plus grande proportion de locataires (56,1 %) de tous les secteurs à faible ",
                      "potentiel de vie active, suivie de Mirabel avec 29,3 %.")) %>%
  add_row(en = paste0("Kirkland has the lowest average rent ($125) of all areas with low walkability."),
          fr = paste0("Kirkland a le loyer moyen le plus bas (125 $) de toutes les zones à faible potentiel piétonnier.")) %>%
  add_row(en = paste0("Mercier has the highest average rent of almost $900 per month of all areas with low walkability, ",
                      "followed by Sainte-Julie with average rent of $875."),
          fr = paste0("De tous les quartiers à faible potentiel piétionnier, Mercier a le loyer moyen le plus élevé, ",
                      "soit près de 900 $ par mois, suivi de Sainte-Julie avec un loyer moyen de 875 $.")) %>%
  add_row(en = paste0("Villeray-Saint-Michel-Parc-Extension has the lowest average rent ($747) of all areas with high ",
                      "walkability, followed by Mercier-Hochelage-Maisonneuve ($761)."),
          fr = paste0("Villeray-Saint-Michel-Parc-Extension a le loyer moyen le plus bas (747 $) de tous les secteurs ",
                      "à forte potentiel piétonnier, suivi de Mercier-Hochelage-Maisonneuve (761 $).")) %>%
  add_row(en = paste0("Senneville has by far the highest average property value ($994,294.4) of all areas with low ",
                      "walkability, and Point-Calumet has the lowest average property value ($223,573.2)."),
          fr = paste0("Senneville a de loin la valeur moyenne des propriétés la plus élevée (994 294,4 $) de toutes ",
                      "les zones à faible potentiel piétonnier, et Point-Calumet a la valeur moyenne de propriété ",
                      "la plus basse (223 573,2 $).")) %>%
  add_row(en = paste0("Mercier-Hochelage-Maisonneuve has the lowest average property value ($337,844.8) of all areas ",
                      "with high active living potential, and Westmount has the highest value ($1,242,703.7)."),
          fr = paste0("Mercier-Hochelage-Maisonneuve a la plus basse valeur moyenne des propriétés (337 844,8 $) ",
                      "de tous les secteurs à fort potentiel de vie active, et Westmount a la valeur la plus élevée ",
                      "(1 242 703,7 $).")) %>%
  add_row(en = paste0("Montréal-Est has the highest proportion of unaffordable housing (29.4%) of all areas with ",
                      "low active living potential, and Sainte-Anne-de-Bellevue has the lowest proportion (10.1%)."),
          fr = paste0("Montréal-Est a la plus forte proportion de logements inabordables (29,4 %) de tous les ",
                      "secteurs à faible potentiel de vie active, et Sainte-Anne-de-Bellevue a la plus faible ",
                      "proportion (10,1 %).")) %>%
  add_row(en = paste0("Ville-Marie has the highest proportion of unaffordable housing (43.3%) of all areas with ",
                      "high active living potential, and Hampstead has the lowest (24.0%)."),
          fr = paste0("Ville-Marie a la plus grande proportion de logements inabordables (43,3 %) de tous les secteurs ",
                      "à fort potentiel de vie active, et Hampstead a la plus petite (24,0 %).")) %>%
  add_row(en = paste0("Côte-des-Neiges-Notre-Dame-de-Grâce has the highest proportion of unsuitable housing (14.2%) ",
                      "of any area with high active living potential, and Westmount has the lowest (4.4%)."),
          fr = paste0("Côte-des-Neiges-Notre-Dame-de-Grâce a la plus grande proportion de logements inadéquats ",
                      "(14,2 %) de tous les secteurs à fort potentiel de vie active, et Westmount a la plus petite (4,4 %).")) %>%
  add_row(en = paste0("The top 3 boroughs that have the highest proportion of people living in census tracts with a ",
                      "low capacity for pedestrian social distancing were 1) Le Plateau-Mont-Royal (74%), 2) ",
                      "Villeray-Saint-Michel-Parc-Extension (65%) and 3) Montréal Nord (60%)."),
          fr = paste0("Les trois arrondissements ayant la plus grande proportion de personnes vivant dans des ",
                      "secteurs de recensement ayant une faible capacité de distanciation sociale des piétons sont 1) ",
                      "Le Plateau-Mont-Royal (74 %), 2) Villeray-Saint-Michel-Parc-Extension (65 %) et 3) Montréal Nord (60 %).")) %>%
  add_row(en = paste0("14.8% of Montreal Agglomeration's total population live in DAs with a pedestrian social ",
                      "distancing capacity below 25%. Of those, 76.31% were not directly serviced by the original May ",
                      "15th expanded active transit plan, and 79.85% were not directly serviced by the revised July 25th plan."),
          fr = paste0("14,8 % de la population totale de l'agglomération de Montréal vit dans des AD dont la capacité de ",
                      "distanciation sociale des piétons est inférieure à 25 %. De ce nombre, 76,31 % n'étaient pas directement ",
                      "desservis par le plan des Voies actives sécuritaires du 15 mai, et 79,85 % n'étaient pas directement desservis ",
                      "par le plan révisé du 25 juillet.")) %>%
  add_row(en = paste0("The top 3 boroughs with the lowest proportion of its population commuting to work by car are Le ",
                      "Plateau-Mont-Royal (24.4%), Ville-Marie (26%) and Côte-des-Neiges-Notre-Dame-de-Grâce (40.6%)."),
          fr = paste0("Les trois arrondissements ayant la plus petite proportion de leur population se rendant ",
                      "au travail en voiture sont Le Plateau-Mont-Royal (24,4 %), Ville-Marie (26 %) et ",
                      "Côte-des-Neiges-Notre-Dame-de-Grâce (40,6 %).")) %>%
  add_row(en = paste0("Montreal’s borough with the highest population density per square meter of walkable space is ",
                      "Côte-des-Neiges-Notre-Dame-de-Grâce (0.28 people / square meters of walkable space)."),
          fr = paste0("L'arrondissement de Montréal ayant la plus forte densité de population par mètre carré de surface ",
                      "accessible pour les piétons est Côte-des-Neiges-Notre-Dame-de-Grâce (0,28 personne / mètre carré de surface marchable).")) %>%
  add_row(en = paste0("DAs with a capacity for pedestrian social distancing less than 25% contain, on average, a net median ",
                      "income of $44,773 compared to $54,930 for the city as a whole and $61,651 for DAs with over 100% capacity."),
          fr = paste0("Les AD ayant une capacité de distanciation sociale des piétons inférieure à 25 % ont, en moyenne, ",
                      "un revenu net médian de 44 773 $, contre 54 930 $ pour l'ensemble de la ville et 61 651 $ pour les AD ",
                      "ayant une capacité supérieure à 100 %.")) %>%
  add_row(en = paste0("DAs with a capacity for pedestrian social distancing less than 25% contain, on average, a 50.4% ",
                      "visible minority population compared to 32% for the city as a whole and 28% for DAs with over ",
                      "100% capacity."),
          fr = paste0("Les AD ayant une capacité de distanciation sociale des piétons inférieure à 25 % contiennent en ",
                      "moyenne 50,4 % de minorités visibles, contre 32 % pour l'ensemble de la ville et 28 % pour les ",
                      "AD ayant une capacité supérieure à 100 %.")) %>%
  add_row(en = paste0("The 3 boroughs or independent municipalities with the highest average 'walkable access to key amenities' ",
                      "index score at the DA level are Le Plateau-Mont-Royal (0.308), Ville-Marie (0.295) ",
                      "and Villeray-Saint-Michel-Parc-Extension (0.237)"),
          fr = paste0("Les 3 arrondissements ou municipalités indépendantes avec les moyennes les plus élevés pour l’accès à pied ",
                      "aux services de base, à l’échelle des AD, sont Le Plateau-Mont-Royal (0.308), Ville-Marie (0.295) et ",
                      "Villeray-Saint-Michel-Parc-Extension (0.237)")) %>%
  add_row(en = paste0("The 3 boroughs or independent municipalities with the lowest average 'walkable access to key amenities' ",
                      "index score at the DA level are: 1) Senneville (0.005); 2) Beaconsfield (0.205); 3) B'aie-d'Urfé (0.022)"),
          fr = paste0("Les 3 arrondissements ou municipalités indépendantes avec les plus basses moyennes pour l’accès à pied aux ",
                      "services de base, à l’échelle des AD, sont Senneville (0.005), Beaconsfield (0.205), B'aie-d'Urfé (0.022)")) %>%
  add_row(en = paste0("Of the boroughs and independent municipalities who's average 'walkable access to key amenities' score at the DA ",
                      "level fall in the top quartile, Côte-des-Neiges-Notre-Dame-de-Grâce has the lowest median capacity for pedestrian ", 
                      "social distancing (35.78%)"),
          fr = paste0("Parmi les arrondissements et municipalités indépendantes dont le score moyen de l’indice de l’accès à pied aux ", 
                      "services de base, à l’échelle des AD, se trouve dans le quartile supérieur, Côte-des-Neiges-Notre-Dame-de-Grâce ", 
                      "à la capacité de distanciation sociale des piétons la plus faible (35.78%)")) %>%
  add_row(en = paste0("Denser neighbourhoods generally have better walkable access to key amenities. A correlation test between the 'walkable ", 
                      "access to key amenities' index score and the log of population density yielded a p value of 0.558, indicating a moderate ", 
                      "to strong positive correlation"),
          fr = paste0("Les quartiers les plus denses ont généralement un meilleur accès à pied aux services de base. Un test de corrélation ", 
                      "entre l’indice d’accès à pied aux services de base et le log de la densité de la population donne une valeur p de 0.558, ", 
                      "indiquant une corrélation positive modérée voire forte entres les deux variables")) %>%
  add_row(en = paste0("DAs with a capacity for pedestrian social distancing less than 25% contain, on average, a 45.25% ",
                      "immigrant population compared to 33% for the city as a whole and 30% for DAs with over 100% capacity."),
          fr = paste0("Les AD ayant une capacité de distanciation sociale des piétons inférieure à 25% contiennent en ",
                      "moyenne 45,25% de population immigrante contre 33% pour l'ensemble de la ville et 30% pour les ",
                      "AD ayant une capacité supérieure à 100%.")) %>%
  add_row(en = paste0("Of the DAs with a capacity for pedestrian social distancing less than 25%, the ones located in the Ville-Marie borough have ", 
                      "the lowest average net median income ($ 13,344)."),
          fr = paste0("Parmi les ADs avec une capacité de distanciation sociale des piétons inférieure à 25%, ceux qui se situent dans l’arrondissement ", 
                      "de Ville-Marie ont, en moyenne, les revenus médians nets les plus bas (13 344$).")) %>%
  add_row(en = paste0("Of the DAs with a capacity for pedestrian social distancing less than 25%, the ones located in the independent municipality ", 
                      "of Beaconsfield have the highest average net median income ($ 43,349)."),
          fr = paste0("Parmi les ADs avec une capacité de distanciation sociale des piétons inférieure à 25%, ceux qui se situent dans la municipalité ", 
                      "indépendante de Beaconsfield ont, en moyenne, les revenus médians nets les plus hauts (43 349$).")) %>%
  add_row(en = paste0("In absolute terms, Côte-des-Neiges-Notre-Dame-de-Grâce is the borough with the highest number of visible minorities (35,055) ", 
                      "living in DAs with less than 25% capacity for pedestrian social distancing."),
          fr = paste0("En termes absolus, Côte-des-Neiges-Notre-Dame-de-Grâce est l’arrondissement avec le plus haut nombre de minorités visibles ", 
                      "(35,055) vivant dans des AD avec moins de 25% de capacité de distanciation sociale des piétons.")) %>%
  add_row(en = paste0("In absolute terms, Outremont is the borough with the lowest number of visible minorities (305) living in DAs with less than ", 
                      "25% capacity for pedestrian social distancing."),
          fr = paste0("En termes absolus, Outremont est l’arrondissement avec le plus bas nombre de minorités visibles (305) vivant dans des AD avec ", 
                      "moins de 25% de capacité de distanciation sociale des piétons.")) %>%
  add_row(en = paste0("In relative terms, Villeray-Saint-Michel-Parc-Extension is the borough with the highest average percentage of visible minorities ", 
                      "(66%) living in DAs with less than 25% capacity for pedestrian social distancing."),
          fr = paste0("En termes relatifs, Villeray-Saint-Michel-Parc-Extension est l’arrondissement avec le plus haut pourcentage moyen de minorités ", 
                      "visibles (66%) vivant dans des AD avec moins de 25% de capacité de distanciation sociale des piétons.")) %>%
  add_row(en = paste0("In relative terms, Outremont is the borough with the lowest average percentage of visible minorities (6.1%) living in DAs with ", 
                      "less than 25% capacity for pedestrian social distancing."),
          fr = paste0("En termes relatifs, Outremont est l’arrondissement avec le plus bas pourcentage moyen de minorités visibles (6.1%) vivant dans ", 
                      "des AD avec moins de 25% de capacité de distanciation sociale des piétons.")) %>%
  add_row(en = paste0("In absolute terms, Côte-des-Neiges-Notre-Dame-de-Grâce is the borough with the highest number of immigrants (32,270) living in ", 
                      "DAs with less than 25% capacity for pedestrian social distancing."),
          fr = paste0("En termes absolus, Côte-des-Neiges-Notre-Dame-de-Grâce est l’arrondissement avec le plus haut nombre d’immigrants (32,270) vivant ", 
                      "dans des AD avec moins de 25% de capacité de distanciation sociale des piétons.")) %>%
  add_row(en = paste0("In absolute terms, L’Île-Bizard-Sainte-Geneviève is the borough with the lowest number of immigrants (480) living in DAs with ", 
                      "less than 25% capacity for pedestrian social distancing."),
          fr = paste0("En termes absolus, L'Île-Bizard-Sainte-Geneviève est l’arrondissement avec le plus bas nombre d’immigrants (480) vivant dans des ", 
                      "AD avec moins de 25% de capacité de distanciation sociale des piétons.")) %>%
  add_row(en = paste0("In relative terms, Saint-Laurent is the borough with the highest average percentage of immigrants (62%) living in DAs with less ", 
                      "than 25% capacity for pedestrian social distancing."),
          fr = paste0("En termes relatifs, Saint-Laurent est l’arrondissement avec le plus haut pourcentage moyen d’immigrants (62%) vivant dans des AD ", 
                      "avec moins de 25% de capacité de distanciation sociale des piétons.")) %>%
  add_row(en = paste0("In relative terms, Sainte-Anne-de-Bellevue (independent municipality) has the lowest average percentage of immigrants (12%) living ", 
                      "in DAs with less than 25% capacity for pedestrian social distancing."),
          fr = paste0("En termes relatifs, Sainte-Anne-de-Bellevue (municipalité indépendante) a le plus bas pourcentage moyen d’immigrants (12%) vivant ", 
                      "dans des AD avec moins de 25% de capacité de distanciation sociale des piétons.")) %>%
  add_row(en = paste0("The top 3 boroughs or independent municipalities in the Montreal agglomeration with the lowest ",
                      "average sidewalk widths are 1) L’Île-Bizard-Sainte-Geneviève (0.48 meters), 2) Pierrefonds-Roxboro ",
                      "(0.67 meters) and 3) Dollard-des-Ormeaux (0.68 meters). More than 75% of trips to work in these ",
                      "three boroughs are done by car."),
          fr = paste0("Les 3 arrondissements ou municipalités indépendantes de l'agglomération de Montréal ayant la ",
                      "plus petite largeur moyenne de trottoir sont 1) L'Île-Bizard-Sainte-Geneviève (0,48 mètre), ",
                      "2) Pierrefonds-Roxboro (0,67 mètre) et 3) Dollard-des-Ormeaux (0,68 mètre). Plus de 75 % des ",
                      "déplacements domicile-travail dans ces trois arrondissements se font en voiture.")) %>%
  add_row(en = paste0("Le Plateau-Mont-Royal is the borough with the highest average sidewalk width at 3.05 meters. ",
                      "However, 35% of boroughs or independent municipalities in Montreal’s agglomeration have ",
                      "lower population densities per square meter of sidewalk space than the Plateau-Mont-Royal. ",
                      "Further, 62% of boroughs or independent municipalities in Montreal's agglomeration have ",
                      "lower population densities per square meter of total walkable space than the ",
                      "Plateau-Mont-Royal. Only 24% of residents in the Plateau-Mont-Royal commute to work by car."),
          fr = paste0("Le Plateau-Mont-Royal est l'arrondissement où la largeur moyenne des trottoirs est la plus ",
                      "élevée, soit 3,05 mètres. Toutefois, 35 % des arrondissements ou des municipalités indépendantes ",
                      "de l'agglomération de Montréal ont une densité de population par mètre carré de trottoir ",
                      "inférieure à celle du Plateau-Mont-Royal. De plus, 62 % des arrondissements ou des municipalités ",
                      "indépendantes de l'agglomération de Montréal ont une densité de population par mètre carré de ",
                      "surface marchable totale inférieure à celle du Plateau-Mont-Royal. Seulement 24 % des résidents ",
                      "du Plateau-Mont-Royal se rendent au travail en voiture.")) %>%
  add_row(en = paste0("The highest average rent in Montreal is in Westmount, at $1269.1 per month, followed by ",
                      "Outremont at $1137.1 per month."),
          fr = paste0("Le loyer moyen le plus élevé à Montréal se trouve à Westmount, à 1269,1 $ par mois, suivi ",
                      "d'Outremont, à 1177,1 $ par mois.")) %>%
  add_row(en = paste0("The lowest average rent in the Montreal CMA is in Beaconsfield, at $237.4 per month, followed ", 
                      "by Rosemère ($276.2) and Boucherville ($362.2). Within the City of Montreal, the borough with ",
                      "the lowest average rent is L'Île-Bizard-Sainte-Geneviève, at $432.9 per month, followed by ",
                      "Pierrefonds-Roxboro ($545.6) and Rivière-des-Prairies-Pointe-aux-Trembles ($610.8)."),
          fr = paste0("Le loyer moyen le plus bas de la RMR de Montréal se trouve à Beaconsfield, avec 237,4 $ par ",
                      "mois, suivi de Rosemère (276,2 $) et de Boucherville (362,2 $). Au sein de la Ville de Montréal, ",
                      "l'arrondissement où le loyer moyen est le plus bas est celui de L'Île-Bizard-Sainte-Geneviève, avec ",
                      "432,9 $ par mois, suivi de Pierrefonds-Roxboro (545,6 $) et de Rivière-des-Prairies-Pointe-aux-Trembles ",
                      "(610,8 $).")) %>%
  add_row(en = paste0("Westmount has the highest average property value ($1,242,703.7), followed by Hampstead ",
                      "($1,125,655.9)."),
          fr = paste0("Westmount a la valeur moyenne des propriétés la plus élevée (1 242 703,7 $), suivie de ",
                      "Hampstead (1 125 655,9 $).")) %>%
  add_row(en = paste0("The lowest average property value in the Montreal CMA ($223,573.2) can be found in Pointe-Calumet."),
          fr = paste0("La valeur moyenne des propriétés la plus basse dans la RMR de Montréal (223 573,2 $) se trouve à Pointe-Calumet.")) %>%
  add_row(en = paste0("Baie-d'Urfé has the lowest proportion of tenants, at 2.6%, followed by Lorraine with 2.7%."),
          fr = paste0("Baie-d'Urfé a la plus petite proportion de locataires, avec 2,6%, suivie par la Lorraine avec 2,7%.")) %>%
  add_row(en = paste0("Côte-des-Neiges-Notre-Dame-de-Grâce has the highest proportion of tenants (73.7%), closely ",
                      "followed by Ville-Marie (73.0%) and Le Plateau-Mont-Royal (72.8%)."),
          fr = paste0("Côte-des-Neiges-Notre-Dame-de-Grâce a la plus grande proportion de locataires (73,7 %), suivie ",
                      "de près par Ville-Marie (73,0 %) et Le Plateau-Mont-Royal (72,8 %).")) %>%
  add_row(en = paste0("The top 10th percentile of tenant proportion in Montreal falls above 64.7%."),
          fr = paste0("Le 90e percentile de la proportion de locataires à Montréal se situe au-dessus de 64,7 %.")) %>%
  add_row(en = paste0("Sainte-Anne-de-Bellevue has the lowest proportion of unaffordable housing at 10.1%, followed ",
                      "by Mercier with 10.3% unaffordable housing."),
          fr = paste0("Sainte-Anne-de-Bellevue a la plus petite proportion de logements inabordables avec ",
                      "10,1%, suivie de Mercier avec 10,3% de logements inabordables.")) %>%
  add_row(en = paste0("Ville-Marie has the highest proportion of unaffordable housing at 43.3%. Le Plateau-Mont-Royal ",
                      "falls just behind with 38.1% unaffordable housing."),
          fr = paste0("C'est à Ville-Marie que l'on trouve la plus forte proportion de logements inabordables, ",
                      "soit 43,3 %. Le Plateau-Mont-Royal se situe juste derrière avec 38,1 % de logements inabordables.")) %>%
  add_row(en = paste0("The top 10 percentile of Montreal has more than 31.2% unaffordable housing."),
          fr = paste0("Le 90e percentile de Montréal compte plus de 31,2 % de logements inabordables.")) %>%
  add_row(en = paste0("Sainte-Anne-de-Bellevue, Saint-Bruno-de-Montarville, Sainte-Julie, and Léry ",
                      "have 0% unsuitable housing."),
          fr = paste0("Sainte-Anne-de-Bellevue, Saint-Bruno-de-Montarville, Sainte-Julie et Léry ont 0% de ",
                      "logements de taille non convenable.")) %>%
  add_row(en = paste0("Saint-Laurent has the highest proportion of unsuitable housing at 15.0%, followed by ",
                      "Côte-des-Neiges-Notre-Dame-de-Grâce at 14.2%."),
          fr = paste0("Saint-Laurent présente la plus forte proportion de logements de taille non convenable avec ",
                      "15,0 %, suivi de Côte-des-Neiges-Notre-Dame-de-Grâce avec 14,2 %.")) %>%
  add_row(en = paste0("Half of areas in Montreal have more than 2.9% unsuitable housing, 30% have more than 5.2% ",
                      "unsuitable housing, and 10% have more than 8.2% unsuitable housing."),
          fr = paste0("La moitié des quartiers de Montréal ont plus de 2,9 % de logements de taille non convenable, ",
                      "30 % ont plus de 5,2 % de logements de taille non convenable et 10 % en ont plus de 8,2 %.")) %>%
  add_row(en = paste0("Montréal-Nord has the highest proportion of people with an annual income of less than $50,000 ",
                      "at 59.5%, closely followed by Villeray-Saint-Michel-Parc-Extension (57.7%) and Ville-Marie (57.6%)."),
          fr = paste0("Montréal-Nord compte la plus grande proportion de personnes ayant un revenu annuel inférieur ",
                      "à 50 000 $, soit 59,5 %, suivie de près par Villeray-Saint-Michel-Parc-Extension (57,7 %) ",
                      "et Ville-Marie (57,6 %).")) %>%
  add_row(en = paste0("Sainte-Anne-de-Bellevue has the lowest proportion of people with an annual income of less than ",
                      "$50,000 at 7.4%."),
          fr = paste0("Sainte-Anne-de-Bellevue a la plus petite proportion de personnes ayant un revenu annuel ",
                      "inférieur à 50 000 $, soit 7,4 %.")) %>%
  add_row(en = paste0("Sainte-Anne-de-Bellevue has the highest proportion of people with an annual income of ",
                      "more than $100,000 at 65.4%, and Montreal-Nord has the lowest (9.7%)."),
          fr = paste0("Sainte-Anne-de-Bellevue a la plus grande proportion de personnes ayant un revenu annuel ",
                      "supérieur à 100 000 $, soit 65,4 %, et Montréal-Nord a la plus petite (9,7 %).")) %>%
  add_row(en = paste0("Sainte-Anne-des-Plaines has the highest proportion of people (43.7%) with an annual income of ",
                      "between $50,000 and $100,000."),
          fr = paste0("Sainte-Anne-des-Plaines compte la plus grande proportion de personnes (43,7 %) ayant un revenu ",
                      "annuel compris entre 50 000 et 100 000 dollars.")) %>%
  add_row(en = paste0("Half of the Montreal CMA's boroughs or cities has a proportion of 29.0% or less people with an ",
                      "annual income below $50,000. 10% of the Montreal CMA's borough or cities has a proportion of 49.2% ",
                      "or higher of people with an annual income below $50,000."),
          fr = paste0("La moitié des arrondissements ou des villes de la RMR de Montréal comptent une proportion de 29,0 % ",
                      "ou moins de personnes ayant un revenu annuel inférieur à 50 000 $. 10 % des arrondissements ou des ",
                      "villes de la RMR de Montréal ont une proportion de 49,2 % ou plus de personnes ayant un revenu annuel ",
                      "inférieur à 50 000 $.")) %>%
  add_row(en = paste0("Montreal-Nord has the lowest median income in Montreal ($43,240.1), followed by Villeray-",
                      "Saint-Michel-Parc-Extension ($43,910.8)."),
          fr = paste0("Montréal-Nord a le revenu médian le plus bas de Montréal (43 240,1 $), suivi de ",
                      "Villeray-Saint-Michel-Parc-Extension (43 910,8 $).")) %>%
  add_row(en = paste0("Hampstead has the highest median income in Montreal ($134,155.3), followed by Westmount ($133,333.9), ",
                      "Sainte-Anne-de-Bellevue ($130,552.9), Mont-Royal ($130,019.4), and Beaconsfield ($129,911.4)."),
          fr = paste0("Hampstead a le revenu médian le plus élevé à Montréal (134 155,3 $), suivi de Westmount (133 333,9 $), ",
                      "Sainte-Anne-de-Bellevue (130 552,9 $), Mont-Royal (130 019,4 $) et Beaconsfield (129 911,4 $).")) %>%
  add_row(en = paste0("Sainte-Julie, Notre-Dame-de-l'Île-Perrot, Oka, Pointe-Calumet, and Charlemagne have less than ",
                      "0.2% new immigrants."),
          fr = paste0("Sainte-Julie, Notre-Dame-de-l'Île-Perrot, Oka, Pointe-Calumet, et Charlemagne ont moins de 0.2% ",
                      "de nouveaux immigrants.")) %>%
  add_row(en = paste0("Sainte-Julie has 0.0% immigrants, and Pointe-Calumet has the next lowest proportion at 2.1%."),
          fr = paste0("Sainte-Julie compte 0,0 % d'immigrants, et Pointe-Calumet a la deuxième plus basse proportion ",
                      "avec 2,1 %.")) %>%
  add_row(en = paste0("Saint-Laurent has the highest proportion of new immigrants (12.1%), followed by ",
                      "Côte-des-Neiges-Notre-Dame-de-Grâce (12.0%)."),
          fr = paste0("Saint-Laurent a la plus grande proportion de nouveaux immigrants (12,1 %), suivie de ",
                      "Côte-des-Neiges-Notre-Dame-de-Grâce (12,0 %).")) %>%
  add_row(en = paste0("Saint-Laurent has the highest proportion of immigrants (53.6%) and also the highest proportion ",
                      "of new immigrants (12.1%)."),
          fr = paste0("Saint-Laurent a la plus grande proportion d'immigrants (53,6 %) et aussi la plus grande proportion ",
                      "de nouveaux immigrants (12,1 %).")) %>%
  add_row(en = paste0("Half of areas in Montreal has a proportion of immigrants 22.6% of higher (50th percentile)."),
          fr = paste0("La moitié des quartiers de Montréal ont une proportion d'immigrants de 22,6 % ou plus (50e percentile).")) %>%
  add_row(en = paste0("50% of areas in Montreal have new immigrant proportions of 2.8% or lower, and 10% have ",
                      "proportions above 8.3%."),
          fr = paste0("50 % des quartiers de Montréal ont une proportion de nouveaux immigrants de 2,8 % ou moins, ",
                      "et 10 % ont une proportion supérieure à 8,3 %.")) %>%
  add_row(en = paste0("The proportion of people who walk or bike to work in Montreal ranges between 0.0% (Saint-Isidore) ",
                      "and 36.0% (Le Plateau-Mont-Royal)."),
          fr = paste0("La proportion de personnes qui se rendent au travail à Montréal à pied ou à vélo varie entre ",
                      "0,0 % (Saint-Isidore) et 36,0 % (Le Plateau-Mont-Royal).")) %>%
  add_row(en = paste0("50% of areas in Montreal have proportions of people who walk or bike to work lower than 4.4%, ",
                      "and 10% of areas have proportions higher than 11.7%."),
          fr = paste0("50 % des quartiers de Montréal ont des proportions de personnes qui se rendent au travail à ",
                      "pied ou à vélo inférieures à 4,4 %, et 10 % des quartiers ont des proportions supérieures à 11,7 %.")) %>%
  add_row(en = paste0("The proportion of people who take public tansit to work in Montreal ranges between 3.0% ",
                      "(Mirabel) and 46.7% (Côte-des-Neiges-Notre-Dame-de-Grâce)."),
          fr = paste0("La proportion de personnes qui prennent le transport en commun pour aller travailler à Montréal ",
                      "varie entre 3,0 % (Mirabel) et 46,7 % (Côte-des-Neiges-Notre-Dame-de-Grâce).")) %>%
  add_row(en = paste0("Côte-des-Neiges-Notre-Dame-de-Grâce has the highest proportion of people who take public ",
                      "transit to work (46.7%), followed by Villeray-Saint-Michel-Parc-Extension (45.2%)."),
          fr = paste0("Côte-des-Neiges-Notre-Dame-de-Grâce compte la plus grande proportion de personnes ",
                      "qui prennent le transport en commun pour se rendre au travail (46,7 %), suivi de ",
                      "Villeray-Saint-Michel-Parc-Extension (45,2 %).")) %>%
  add_row(en = paste0("50% of areas in Montreal have proportions of people who take transit to work lower than 16.8%."),
          fr = paste0("Dans 50 % des quartiers de Montréal, la proportion de personnes qui prennent le transport en ",
                      "commun pour se rendre au travail est inférieure à 16,8 %.")) %>%
  add_row(en = paste0("10% of areas in Montreal have proportions of people taking transit to work higher than 36.0%."),
          fr = paste0("Dans 10 % des quartiers de Montréal, la proportion de personnes qui prennent le transport ",
                      "en commun pour se rendre au travail est supérieure à 36,0 %.")) %>%
  add_row(en = paste0("The proportion of people who drive to work ranges from 24.9% (Le Plateau-Mont-Royal) and ",
                      "95.7% (Saint-Isidore)."),
          fr = paste0("La proportion de personnes qui se rendent au travail en voiture varie entre 24,9% ",
                      "(Le Plateau-Mont-Royal) et 95,7% (Saint-Isidore).")) %>%
  add_row(en = paste0("90% of areas in Montreal have proportions higher than 48.0% of people who drive to work."),
          fr = paste0("90 % des quartiers de Montréal ont des proportions supérieures à 48,0 % de personnes ",
                      "qui se rendent au travail en voiture.")) %>%
  add_row(en = paste0("The three areas with the lowest property values all have average Active Living Environment ",
                      "classes of 2 or less (Pointe-Calumet (1.7); Sainte-Anne-des-Plaines (1.8); Bois-des-Filion (2.0))."),
          fr = paste0("Les trois régions dont la valeur des propriétés est la plus faible ont toutes des moyennes de classe de","
                      potentiel de vie active de 2 ou moins (Pointe-Calumet (1,7) ; Sainte-Anne-des-Plaines ",
                      "(1,8) ; Bois-des-Filion (2,0)).")) %>%
  add_row(en = paste0("The ten areas with the highest proportion of tenants have active living environment class ",
                      "averages ranging from 3.96 to 5."),
          fr = paste0("Les dix zones ayant la plus forte proportion de locataires ont des moyennes de classe de potentiel de ",
                      "vie active allant de 3,96 à 5.")) %>%
  add_row(en = paste0("The ten areas with the lowest proportion of tenant have average active living environments ",
                      "classes ranging from 1 and 2."),
          fr = paste0("Les dix zones ayant la plus faible proportion de locataires ont des moyennes de classe de ",
                      "potentiel de vie active allant de 1 à 2.")) %>%
  add_row(en = paste0("The areas with the lowest active living potential (ALE class 1) have tenant proportions ",
                      "ranging from 0.0% (Saint-Bruno-de-Montarville, Oka) to 24.6% (Saint-Isidore)."),
          fr = paste0("Les zones ayant le plus faible potentiel de vie active (classe 1 de l'AVA) ont des proportions ",
                      "de locataires allant de 0,0% (Saint-Bruno-de-Montarville, Oka) à 24,6% (Saint-Isidore).")) %>%
  add_row(en = paste0("The areas with the lowest active living potential (ALE class 1) have average property values ",
                      "ranging from $250,046.0 (Saint-Isidore) to $994,294.4 (Saint-Anne-de-Bellevue)."),
          fr = paste0("Les zones ayant le plus faible potentiel de vie active (classe 1 de l'AVA) ont une valeur ",
                      "moyenne des propriétés allant de 250 046,0 $ (Saint-Isidore) à 994 294,4 $ (Saint-Anne-de-Bellevue).")) %>%
  add_row(en = paste0("The areas with the lowest active living potential (ALE class 1) have proportions of unaffordable ",
                      "housing ranging from 10.1% (Sainte-Anne-de-Bellevue) to 18.2% (Sainte-Julie)."),
          fr = paste0("Les zones ayant le plus faible potentiel de vie active (classe 1 de l'AVA) présentent des proportions ",
                      "de logements inabordables allant de 10,1% (Sainte-Anne-de-Bellevue) à 18,2% (Sainte-Julie).")) %>%
  add_row(en = paste0("The areas with the lowest active living potential (ALE class 1) have proportions of unsuitable ",
                      "housing ranging from 0.0% (Sainte-Julie, Saint-Bruno-de-Montarville, Sainte-Anne-de-Bellevue) ",
                      "to 5.1% (Oka)."),
          fr = paste0("Les zones ayant le plus faible potentiel de vie active (classe 1 de l'AVA) présentent des proportions ",
                      "de logements de taille non convenable allant de 0,0% (Sainte-Julie, Saint-Bruno-de-Montarville, ",
                      "Sainte-Anne-de-Bellevue) à 5,1% (Oka).")) %>%
  add_row(en = paste0("Sainte-Anne-de-Bellevue has the highest proportion of people with an annual income of more than ",
                      "$100,000 at 65.4%, and Montreal-Nord has the lowest (9.7%)."),
          fr = paste0("Sainte-Anne-de-Bellevue a la plus forte proportion de personnes ayant un revenu annuel supérieur ",
                      "à 100 000 $, soit 65,4 %, et Montréal-Nord a la plus faible (9,7 %).")) %>%
  add_row(en = paste0("Sainte-Anne-des-Plaines has the highest proportion of people (43.7%) with an annual income ",
                      "of up to $100,000."),
          fr = paste0("Sainte-Anne-des-Plaines compte la plus forte proportion de personnes (43,7 %) ayant un ",
                      "revenu annuel allant jusqu'à 100 000 $.")) %>%
  add_row(en = paste0("The areas with the lowest active living potential (ALE class 1) have median household incomes ",
                      "ranging from $63,744.0 (Saint-Isidore) to $130,552.9 (Sainte-Anne-de-Bellevue)."),
          fr = paste0("Les régions où le potentiel de vie active est le plus faible (classe 1 de l'AVA) ont des ",
                      "revenus des ménages médians allant de 63 744,0 $ (Saint-Isidore) à 130 552,9 $ ",
                      "(Sainte-Anne-de-Bellevue).")) %>%
  add_row(en = paste0("The areas with the highest active living potential (average ALE class 5 or higher) have ",
                      "median household incomes ranging from $43,910.8 (Villeray-Saint-Michel-Parc-Extension) to ",
                      "$134,155.3 (Hampstead)."),
          fr = paste0("Les régions où le potentiel de vie active est le plus élevé (moyenne de l'AVA classe 5 ",
                      "ou plus) ont des revenus médians par ménage allant de 43 910,8 $ (Villeray-Saint-Michel-Parc-Extension) ",
                      "à 134 155,3 $ (Hampstead).")) %>%
  add_row(en = paste0("The areas with the lowest proportion of people with commute times of more than one hour ",
                      "are Westmount (5.5%), Outremont (5.7%), and Mont-Royal (5.9%)."),
          fr = paste0("Les régions où la proportion de personnes ayant un temps de déplacement domicile-travail de plus d'une ",
                      "heure est la plus faible sont Westmount (5,5 %), Outremont (5,7 %) et Mont-Royal (5,9 %).")) %>%
  add_row(en = paste0("The areas with the highest proportion of people with commute times of more than one hour ",
                      "are Léry (19.2%), Saint-Léonard (16.6%), and Anjou (16.2%)."),
          fr = paste0("Les régions où la proportion de personnes ayant un temps de déplacement domicile-travail supérieur à une heure est ",
                      "la plus élevée sont Léry (19,2%), Saint-Léonard (16,6%) et Anjou (16,2%).")) %>%
  add_row(en = paste0("The three areas with the highest proportion of people with commute times of more than one hour ",
                      "have varying classes of active living potential, from 1 (Léry) to 3.9 (Saint-Léonard)."),
          fr = paste0("Les trois zones qui comptent la plus forte proportion de personnes ayant un temps de trajet ",
                      "de plus d'une heure ont des classes de potentiel de vie active variables, de 1 (Léry) à ",
                      "3,9 (Saint-Léonard).")) %>%
  add_row(en = paste0("The three areas with the lowest proportion of people with commute times of more than one ",
                      "hour all have average classes of active living potential above 3 (Westmount: 4.36, ",
                      "Outremont: 4.43, Mont-Royal: 3.33)."),
          fr = paste0("Les trois régions qui comptent la plus faible proportion de personnes dont le temps ",
                      "de déplacement est supérieur à une heure ont toutes des classes moyennes de potentiel ",
                      "de vie active supérieures à 3 (Westmount: 4,36, Outremont: 4,43, Mont-Royal: 3,33).")) %>%
  add_row(en = paste0("The five areas with the highest active living potential are all boroughs of the ",
                      "City of Montreal (Le Plateau-Mont-Royal; Rosemont-La-Petite-Patrie; Ville-Marie; ",
                      "Villeray-Saint-Michel-Parc-Extension; Côte-des-Niges-Notre-Dame-de-Grâce)."),
          fr = paste0("Les cinq secteurs ayant le plus fort potentiel de vie active sont tous des arrondissements ",
                      "de la Ville de Montréal (Le Plateau-Mont-Royal; Rosemont-La-Petite-Patrie; Ville-Marie; ",
                      "Villeray-Saint-Michel-Parc-Extension; Côte-des-Niges-Notre-Dame-de-Grâce).")) %>%
  
  # For the waiter
  add_row(en = "Please wait, this may take a few minutes",
          fr = "Veuillez patienter, ceci peut prendre quelques minutes") %>% 
  
  # Accessibility module
  add_row(en = "Access to Urban Opportunities",
          fr = "Accès aux opportunités urbaines") %>% 
  add_row(en = "Dissemination Area",
          fr = "Aire de diffusion") %>% 
  add_row(en = "Route Planner",
          fr = "Planificateur d'itinéraire") %>% 
  add_row(en = "Cycling Network",
          fr = "Réseau cycliste") %>% 
  
  # Accessibility destionations
  add_row(en = "Select your destination",
          fr = "Sélectionnez votre destination") %>%
  add_row(en = "COVID-19 Testing Centre",
          fr = "Centre de dépistage COVID-19") %>%
  add_row(en = "Health Care",
          fr = "Soins de santé") %>%
  add_row(en = "Grocery Store",
          fr = "Épicerie") %>%
  add_row(en = "Pharmacy",
          fr = "Pharmacie") %>%
  add_row(en = "Eating Place",
          fr = "Lieu de restauration") %>%
  add_row(en = "Shortest Route",
          fr = "Itinéraire le plus court") %>%
  add_row(en = "Shortest Route",
          fr = "Safest Route") %>%
  add_row(en = "Route Information",
          fr = "Informations sur l'itinéraire") %>%
  add_row(en = "Elevation Profile",
          fr = "Profil d'élévation") %>%
  add_row(en = "Select a variable:",
          fr = "Choisissez une variable:") %>%
  add_row(en = "Travel Time to Closest Health Care",
          fr = "Temps de trajet pour se rendre aux soins de santé les plus proches") %>%
  add_row(en = "Travel Time to Closest Grocery",
          fr = "Temps de trajet jusqu'à l'épicerie la plus proche") %>%
  add_row(en = "Travel Time to Closest Pharmacy",
          fr = "Temps de trajet jusqu'à la pharmacie la plus proche") %>%
  add_row(en = "Number of Accessible Eating Places",
          fr = "Nombre de lieux de restauration accessibles") %>%
  add_row(en = "Journey Time",
          fr = "Durée du trajet") %>%
  add_row(en = "Route Distance",
          fr = "Distance du trajet") %>%
  add_row(en = "Cycling Facility",
          fr = "Installations pour cyclistes") %>%
  add_row(en = "CO2 Avoided",
          fr = "CO2 évité") %>%
  add_row(en = "Distance (m)",
          fr = "Distance (m)") %>%
  add_row(en = "Elevation (m)",
          fr = "Élevation (m)") %>%
  add_row(en = "km/sq(km)",
          fr = "km/km2") %>%
  add_row(en = "Low Income Rate (2016)",
          fr = "Taux de faible revenu (2016)") %>%
  add_row(en = "Cycling Rate",
          fr = "Taux de cyclisme") %>%
  add_row(en = "Density of Cycling Facility",
          fr = "Densité de l'infrastructure cyclable") %>%
  add_row(en = "Density of Cycling Facility",
          fr = "Densité de l'infrastructure cyclable") %>%
  add_row(en = "Travel Time to Closest Health Care",
          fr = "Temps de trajet jusqu'à l'établissement de soins de santé le plus proche") %>%
  add_row(en = "Travel Time to Closest Grocery",
          fr = "Temps de trajet jusqu'à l'épicerie la plus proche") %>%
  add_row(en = "Travel Time to Closest Pharmacy",
          fr = "Temps de trajet jusqu'à la pharmacie la plus proche") %>%
  add_row(en = "Number of Accessible Eating Places",
          fr = "Nombre de lieux de restauration accessibles") %>%
  add_row(en = "Safest Route/Fastest Route",
          fr = "L'itinéraire le plus sécuritaire/le plus rapide") %>%
  add_row(en = "Travel Time to Closest Health Care Facility (minutes)",
          fr = "Temps de trajet jusqu'à l'établissement de soins de santé le plus proche (en minutes)") %>%
  add_row(en = "Travel Time to Closest Grocery Store (minutes)",
          fr = "Temps de trajet jusqu'à l'épicerie la plus proche (en minutes)") %>%
  add_row(en = "Travel Time to Closest Pharmacy (minutes)",
          fr = "Temps de trajet jusqu'à la pharmacie la plus proche (en minutes)") %>%
  add_row(en = "Number of Eating Places within 15-minutes Cycling",
          fr = "Nombre de lieux de restauration accessibles en 15 minutes de vélo") %>%
  add_row(en = paste0("Residents of the Island live an average of 1.8 km from the nearest health care facility,",
                     "<br/>that works out to a 7-minutes cycling. Overall, 19% of people live more than",
                     "<br/>10 minutes away from their nearest health care facility, while 22% live between 5 and",
                     "<br/>10 minutes away and 59% live less than 5 minutes away. Low-income",
                     "<br/>households are more likely to live closer to the nearest health care facility."),
          fr = paste0("Les résidents de l'Île de Montréal vivent, en moyenne, à 1.8 km de l'établissement de soins de santé le plus proche,",
                      "<br/>ce qui revient à 7 minutes de vélo. Dans l'ensemble, 19% des gens habitent à plus de",
                      "<br/>10 minutes de l'établissement de soins de santé le plus proche, tandis que 22% vivent entre 5 et",
                      "<br/>10 minutes et 59% à moins de 5 minutes. Les ménages à faibles revenus",
                      "<br/>sont plus susceptibles de vivre à proximité de l'établissement de soins de santé le plus proche.")) %>%
  add_row(en = paste0("Residents of the Island live an average of 0.4 km from the nearest grocery store,",
                     "<br/>that works out to a 2-minutes cycling. Overall, 97% of people live less than",
                     "<br/>5 minutes away from their nearest grocery store."),
          fr = paste0("Les résidents de l'Île de Montréal vivent, en moyenne, à 0.4 km de l'épicerie la plus proche,",
                      "<br/>ce qui revient à 2 minutes de vélo. Dans l'ensemble, 97% des personnes vivent à moins de 5",
                      "<br/>minutes de vélo de l'épicerie la plus proche.")) %>%
  add_row(en = paste0("Residents of the Island live an average of 0.7 km from the nearest pharmacy,",
                      "<br/>that works out to a 3-minutes cycling. Overall, 92% of people live less than",
                      "<br/>5 minutes away from their nearest pharmacy."),
          fr = paste0("Les résidents de l'Île de Montréal vivent, en moyenne, à 0.7 km de la pharmacie la plus proche,",
                      "<br/>ce qui revient à 3 minutes de vélo. Dans l'ensemble, 92% des personnes vivent à moins de 5",
                      "<br/>minutes de vélo de la pharmacie la plus proche.")) %>%
  add_row(en = "Average rent ($)", fr = "Loyer moyen ($)") %>%
  add_row(en = "Average property value ($)", fr = "Valeur moyenne des logements ($)") %>%
  add_row(en = "Median household income ($)", fr = "Revenu médian des ménages ($)") %>%
  add_row(en = "15-45 minutes to work (%)", fr = "Trajet de 15-45 minutes (%)") %>%
  add_row(en = "More than 45 minutes to work (%)", fr = "Trajet de plus de 45 minutes (%)") %>%
  add_row(en = "Destructive storms", fr = "Tempêtes destructrices") %>%
  add_row(en = "Drought", fr = "Sécheresses") %>%
  add_row(en = "Flood", fr = "Inondations") %>%
  add_row(en = "Heat wave", fr = "Vagues de chaleur") %>%
  add_row(en = "Heavy rain", fr = "Pluies abondantes") %>%
  add_row(en = "250-metre grid", fr = "Cellule carrée de 250m sur 250m") %>%
  add_row(en = "Boroughs and municipalities", fr = "Arrondissements et municipalités") %>%
  add_row(en = "{selection$name_2} of {place_name}", fr = "{selection$name_2} de {place_name}") %>%
  add_row(en = "250-m", fr = "250-m") %>%
  add_row(en = "areas", fr = "aires") %>%
  add_row(en = "Insignificant", fr = "Insignifiant") %>%
  add_row(en = "Minor", fr = "Mineur") %>%
  add_row(en = "Moderate", fr = "Modéré") %>%
  add_row(en = "Elevated", fr = "Élevé") %>%
  add_row(en = "Major", fr = "Majeur") %>%
  add_row(en = "The area around {selection$name}", fr = "La zone entourant {selection$name}") %>%
  add_row(en = "Census tract {selection$name}", fr = "Secteur de recensement {selection$name}") %>%
  add_row(en = "Dissemination area {selection$name}", fr = "Aire de diffusion {selection$name}") %>%
  add_row(en = paste0("At the {z$scale_singular} scale, {z$exp_left} varies from '{z$min_val}' to ",
                      "'{z$max_val}'. A plurality of {z$scale_plural} ({z$mode_prop}) have a value ",
                      "of '{z$mode_val}', while {z$mode_prop_2} have a value of '{z$mode_val_2}'."),
          fr = paste0("À l'échelle {z$scale_singular}, {z$exp_left} varie de '{z$min_val}' à ",
                      "'{z$max_val}'. Une pluralité des {z$scale_plural} ({z$mode_prop}) ont une valeur ",
                      "de '{z$mode_val}', alors que {z$mode_prop_2} ont une valeur de '{z$mode_val_2}'.")) %>%
  add_row(en = paste0("At the {z$scale_singular} scale, {z$exp_left} varies from {z$min_val} to {z$max_val}, ",
                      "with an average value of {z$mean_val} and a median value of {z$median_val}. Two thirds ",
                      "of {z$scale_plural} have a score between {z$quant_low} and {z$quant_high}."),
          fr = paste0("À l'échelle {z$scale_singular}, {z$exp_left} varie de {z$min_val} à {z$max_val}, ",
                      "avec une valeur moyenne de {z$mean_val} et une valeur médiane de {z$median_val}. Deux tiers ",
                      "des {z$scale_plural} ont un score entre {z$quant_low} et {z$quant_high}.")) %>%
  add_row(en = paste0("Climate change will have increasingly negative impacts on communities across ",
                      "Montreal and Canada, affecting different populations in distinct ways and at varying ",
                      "rates. The City of Montreal did an exercise examining the risk of five climate risks, ",
                      "including heat waves, flooding, heavy rain, drought, and destructive storms, visualised here."),
          fr = paste0("Les changements climatiques auront des impacts de plus en plus négatifs sur les communautés ",
                      "de Montréal et du Canada, affectant différentes populations de manière distincte, et ",
                      "à des rythmes différents. La Ville de Montréal a examiné cinq risques climatiques, dont ",
                      "les vagues de chaleur, les inondations, les pluies abondantes, la sécheresse et les tempêtes ",
                      "destructrices, visualisés ici.")) %>%
  add_row(en = "Drought vulnerability", fr = "Vulnérabilité aux sécheresses") %>%
  add_row(en = "the vulnerability to climate-change related drought events", 
          fr = "la vulnérabilité aux sécheresses liées aux changements climatiques") %>%
  add_row(en = "Flood vulnerability", fr = "Vulnérabilité aux crues") %>%
  add_row(en = "the vulnerability to climate-change related flooding events", 
          fr = "la vulnérabilité aux crues liées aux changements climatiques") %>%
  add_row(en = "Heavy rain vulnerability", fr = "Vulnérabilité aux pluies abondantes") %>%
  add_row(en = "the vulnerability to climate-change related heavy rain events", 
          fr = "la vulnérabilité aux pluies abondantes liées aux changements climatiques") %>%
  add_row(en = "Destructive storm vulnerability", fr = "Vulnérabilité aux tempêtes destructrices") %>%
  add_row(en = "the vulnerability to climate-change related destructive storm events", 
          fr = "la vulnérabilité aux tempêtes destructrices liées aux changements climatiques") %>%
  add_row(en = "Heat wave vulnerability", fr = "Vulnérabilité aux vagues de chaleur") %>%
  add_row(en = "the vulnerability to climate-change related heat wave events", 
          fr = "la vulnérabilité aux vagues de chaleur liées aux changements climatiques") %>%
  add_row(en = paste0("<p>The Climate Change Risk datasets are part of the start of the City of Montreal’s iterative ",
                      "process examining potential climate risks for the Montreal region. It includes cartographic ",
                      "representation of five areas: heat waves, flooding, heavy rain, drought, and destructive storms. ",
                      "These climate risks, along with increased temperatures, constitute the primary concerns for ",
                      "the Montreal agglomeration as determined in the 2015-2020 Urban Agglomeration Climate Change ",
                      "Adaptation Plan. The plan includes climate modeling and, projections, and potential impacts on ",
                      "buildings, on municipal operations, as well as environmental and socio-economic impacts. The ",
                      "adaptation plan incorporates several of the City’s other climate-related plans and strategies, ",
                      "and focuses on moving from theory to action, taking into consideration Montreal’s landscape and ",
                      "surroundings. The datasets visualised here are publicly available through the Montreal Open Data ",
                      "Portal. <ul> <li>Heat waves include a range of extreme heat events based on temperature and ",
                      "duration. Montreal has generally seen an upward trend in extreme heat events, most noticeably ",
                      "during the 2000s. Heat waves are especially of concern in Montreal due to more than one quarter ",
                      "(28%) of the island containing heat islands. <li>Flooding, specifically river flooding, refers to ",
                      "flow rate or river level exceeding the critical threshold. The Montreal agglomeration’s flooding ",
                      "concerns primarily locations along the Des Prairies River. <li>Heavy rain includes long term heavy ",
                      "rains, potentially causing rivers to overflow, and short-term heavy rain, often localised, can ",
                      "put strain on infrastructures, cause public health problems, and negatively impact certain natural ",
                      "environments. Episodes of heavy rain are on the upward trend in Quebec, with increased instances ",
                      "observed from 1934 to 2014. <li>Drought includes meteorological drought (amount of precipitation), ",
                      "agricultural drought (soil dryness), hydrological drought (surface and groundwaters), and socioeconomic ",
                      "drought (actions of humans on water resources). Montreal has seen a very slight upward trend in ",
                      "meteorological droughts. <li>Destructive storms include wind storms, hail storms, heavy snowstorms, ",
                      "and freezing rain. Events of freezing rain increased 26% from 1979 to 2008, and heavy snowstorms ",
                      "increased over the past 70 years. No assessment could so far be made in regards to the other storm ",
                      "types.</ul>"),
          fr = paste0("<p>Les données sur la vulnérabilité aux changements climatiques font partie des premières étapes ",
                      "du processus itératif de la Ville de Montréal examinant les risques climatiques potentiels pour ",
                      "la région de Montréal. La base de données comprend la représentation cartographique des aléas ",
                      "climatiques suivants: les vagues de chaleur, les crues, les pluies abondantes, la sécheresse et ",
                      "les tempêtes destructrices. Ces risques climatiques, ainsi que l'augmentation des températures, ",
                      "constituent les principales préoccupations de l'agglomération de Montréal, telles que déterminées ",
                      "dans le Plan d'adaptation aux changements climatiques de l'agglomération de Montréal 2015-2020. Le ",
                      "plan comprend des modélisations et des projections climatiques, ainsi que les impacts potentiels sur ",
                      "les bâtiments, sur les opérations municipales, ainsi que les impacts environnementaux et socio-économiques. ",
                      "Le plan d'adaptation intègre plusieurs autres plans et stratégies de la ville liés au climat et vise à passer ",
                      "de la théorie à l'action, en tenant compte du paysage et de l'environnement de Montréal. Les données ",
                      "visualisées ici sont accessibles au public par le biais du portail de données ouvertes de Montréal. ",
                      "<ul> <li>Les vagues de chaleur comprennent une gamme d'événements de chaleur extrême basés sur la ",
                      "température et la durée. Montréal a généralement connu une tendance à la hausse des événements de ",
                      "chaleur extrême, plus particulièrement au cours des années 2000. Les vagues de chaleur sont particulièrement ",
                      "préoccupantes à Montréal en raison du fait que plus du quart (28 %) de l'île contient des îlots de chaleur. ",
                      "<li>Les crues, ou les inondations fluviales, désignent un débit ou un niveau de rivière dépassant le seuil ",
                      "critique. Les inondations de l'agglomération de Montréal concernent principalement les lieux situés le long ",
                      "de la rivière des Prairies. <li>Les pluies abondantes comprennent les pluies abondantes à long terme, pouvant ",
                      "potentiellement faire déborder les rivières; et les pluies abondantes à court terme, souvent localisées, pouvant ",
                      "mettre à mal les infrastructures, causer des problèmes de santé publique et avoir un impact négatif sur certains ",
                      "milieux naturels. Les épisodes de fortes pluies ont tendance à augmenter au Québec, avec une augmentation des ",
                      "occurrences observées de 1934 à 2014. <li>La sécheresse comprend la sécheresse météorologique (quantité de ",
                      "précipitations), la sécheresse agricole (sécheresse des sols), la sécheresse hydrologique (eaux de surface ",
                      "et souterraines) et la sécheresse socio-économique (impact des actions des humains sur les ressources en eau). ",
                      "Montréal a connu une très légère tendance à la hausse des sécheresses météorologiques. <li>Les tempêtes destructrices ",
                      "comprennent les tempêtes de vent, les tempêtes de grêle, les fortes tempêtes de neige et la pluie verglaçante. ",
                      "Les événements de pluie verglaçante ont augmenté de 26% de 1979 à 2008, et les fortes tempêtes de neige ont ",
                      "augmenté au cours des 70 dernières années. Aucune évaluation n'a pu être faite jusqu'à présent en ce qui concerne ",
                      "les autres types de tempêtes.</ul>")) %>%
  add_row(en = paste0("<p>Displayed data for <b>{var}</b> is for the closest ",
                      "available year <b>({year_shown})</b>.</p>"),
          fr = paste0("<p>Les données représentées pour <b>{var}</b> sont celles de ",
                      "l'année disponible la plus proche <b>({year_shown})</b>.</p>")) %>% 
  add_row(en = "Covid interventions",
          fr = "Interventions (COVID)") %>%
  add_row(en = "Healthy Urban Features",
          fr = "Caractéristiques d'une ville saine") %>%
  add_row(en = "Housing realm",
          fr = "Domaine du logement") %>%
  add_row(en = "The housing realm",
          fr = "Le domaine du logement") %>%
  add_row(en = "Montréal climate plans",
          fr = "Plans climat de Montréal") %>%
  add_row(en = "Policy",
          fr = "Politiques") %>%
  add_row(en = "Safety",
          fr = "Sécurité") %>%
  add_row(en = "Safety analysis",
          fr = "Analyse de la sécurité") %>%
  add_row(en = "Green alleys",
          fr = "Ruelles vertes") %>%
  add_row(en = "Health",
          fr = "Santé") %>%
  add_row(en = paste0("<strong>{z$place_heading}</strong>",
                      "<p>{z$place_name} has a population of ",
                      "{prettyNum(round(z$selection$population), ',')} and a ", 
                      "{z$title_left} score ({z$exp_left}) of {round(z$poly_value, 2)}, which is ", 
                      "{z$larger_smaller} the region-wide median of {z$median_val}.",
                      "<p>{z$place_name} has a {z$poor_strong} relative score for this indicator, ",
                      "with {z$exp_left} higher than {z$percentile}% ",
                      "of {z$scale_plural} in the Montreal region."),
          fr = paste0("<strong>{z$place_heading}</strong><p>{z$place_name} a une ",
                      "population de {prettyNum(round(z$selection$population), ',')} ",
                      "et un indice {z$title_left} ({z$exp_left}) de {round(z$poly_value, 2)}, ce qui ",
                      "est {z$larger_smaller} la médiane régionale de {z$median_val}.",
                      "<p>{z$place_name} a un indice relativement {z$poor_strong} pour cet indicateur, ",
                      "avec {z$exp_left} supérieur à {z$percentile}% des ",
                      "{z$scale_plural} de la région de Montréal")) %>% 
  add_row(en = "Housing is important",
          fr = "Le logement est important") %>%
  add_row(en = "May 2020",
          fr = "Mai 2020") %>% 
  add_row(en = "July 2020",
          fr = "Juillet 2020") %>% 
  add_row(en = "October 2020",
          fr = "Octobre 2020") %>% 
  add_row(en = "TK 2021",
          fr = "TK 2021") %>% 
  add_row(en = "Total",
          fr = "Total") %>% 
  add_row(en = "Pedestrian",
          fr = "Piéton") %>% 
  add_row(en = "Cyclist",
          fr = "Cycliste") %>% 
  add_row(en = "Other",
          fr = "Autre") %>% 
  add_row(en = "Count",
          fr = "Compte") %>% 
  add_row(en = "Per sq km",
          fr = "Par kilomètre carré") %>% 
  add_row(en = "Per 1000 residents",
          fr = "Pour 1000 résidents") %>% 
  add_row(en = "Introduction",
          fr = "Introduction") %>% 
  add_row(en = "Community and participation",
          fr = "Communauté et participation") %>% 
  add_row(en = "Greening",
          fr = "Verdir") %>% 
  add_row(en = "Food and agriculture",
          fr = "Alimentation et agriculture") %>% 
  add_row(en = "Land use",
          fr = "Utilisation des sols") %>% 
  add_row(en = "Mobility",
          fr = "Mobilité") %>% 
  add_row(en = "Equity",
          fr = "Equité") %>% 
  add_row(en = "Adaptation and resilience",
          fr = "Adaptation et résilience") %>% 
  add_row(en = "Economy",
          fr = "Économie") %>% 
  add_row(en = "Innovation",
          fr = "Innovation") %>% 
  add_row(en = "Regionalism, internationalism and networks",
          fr = "Régionalisme, internationalisme et réseaux") %>% 
  add_row(en = "Sustainability",
          fr = "Durabilité") %>% 
  distinct(en, .keep_all = TRUE)


# Add names of borough and city, untranslated -----------------------------

qload("data/census.qsm")

translation_fr <- 
  translation_fr %>% 
  add_row(en = borough$name,
          fr = borough$name)

rm(borough, DA, CT)


# save --------------------------------------------------------------------

qsave(translation_fr, "data/translation_fr.qs")
write_csv(translation_fr, "translations/translation_fr.csv")

rm(translation_fr)

# Deepl helper
# to_translate <-
#   str_extract_all("Warning: No translation text found for `May 2020`.
# Warning: No translation text found for `July 2020`.
# Warning: No translation text found for `October 2020`.
# Warning: No translation text found for `TK 2021`.
# Warning: No translation text found for `Total`.
# Warning: No translation text found for `Pedestrian`.
# Warning: No translation text found for `Cyclist`.
# Warning: No translation text found for `Other`.
# Warning: No translation text found for `Count`.
# Warning: No translation text found for `Per sq km`.
# Warning: No translation text found for `Per 1000 residents`.
# Warning: No translation text found for `Introduction`.
# Warning: No translation text found for `Community and participation`.
# Warning: No translation text found for `Greening`.
# Warning: No translation text found for `Food and agriculture`.
# Warning: No translation text found for `Land use`.
# Warning: No translation text found for `Mobility`.
# Warning: No translation text found for `Equity`.
# Warning: No translation text found for `Adaptation and resilience`.
# Warning: No translation text found for `Economy`.
# Warning: No translation text found for `Innovation`.
# Warning: No translation text found for `Regionalism, internationalism and networks`.
# Warning: No translation text found for `Sustainability`.", "`.*`") %>%
#   unlist() %>%
#   str_remove_all(., '`')
# 
# glue('add_row(en = "{to_translate}",
#               fr = "{deeplr::toFrench2(to_translate, auth_key = "my_key")}") %>% ')
# 
#   