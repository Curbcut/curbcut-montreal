###### Data export translation #################################################

# form_translation_tibble(readxl::read_xlsx("data_export_translation.xlsx"))

data_export_translated <- 
  tibble(en = character(), fr = character()) |>                                                  
  add_row(en = paste0("No data/metadata for this location."), 
          fr = paste0("Aucune donnée/métadonnée pour cet emplacement.")) |> 
  add_row(en = paste0("Exporting Data"), 
          fr = paste0("Exportation des données")) |> 
  
  # city amenities
  add_row(en = paste0("The indicators of this module represent the number of d",
                      "estinations accessible by walking and cycling from a sp",
                      "ecified origin within a given time (cumulative-opportun",
                      "ities method).<p>Each indicator is calculed at the diss",
                      "emination blocks level, the smallest geographic area de",
                      "fined by Statistics Canada.<p>The defined cutoff times ",
                      "(15-minute walk ; 20-minute bike ride) do not vary thro",
                      "ughout the study area, i.e., the City of Montreal. <p>T",
                      "he characteristics and quality of the destinations are ",
                      "not taken into account.<p>Travel times are calculated u",
                      "sing the r5r ('Rapid Realistic Routing with R5') packag",
                      "e in R.<p>Calculations take into consideration walking ",
                      "and cycling infrastructures as well as streets' slopes.",
                      "<p>The costs of turning and crossing an intersection ar",
                      "e not considered.<p>The data used to calculate the indi",
                      "cators includes open data from the City of Montreal, DM",
                      "TI data, and OpenStreetMap data."), 
          fr = paste0("Les indicateurs de ce module représentent le nombre de d",
                      "estinations accessibles à la marche et à vélo à partir ",
                      "d'une origine spécifiée à l'intérieur d'un seuil de tem",
                      "ps donnée (méthode d’opportunités cumulatives). <p>Chaq",
                      "ue indicateur est calculé au niveau des îlots de diffus",
                      "ion, soit les plus petites unités spatiales définies pa",
                      "r Statistique Canada.<p>Les seuils de temps choisis (ma",
                      "rche : 15 minutes ; vélo : 20 minutes) sont homogènes s",
                      "ur tout le territoire à l'étude, soit celui de la Ville",
                      " de  Montréal. <p>Les caractéristiques et la qualité de",
                      "s destinations ne sont pas prises en compte. <p>Les tem",
                      "ps de déplacement sont calculés avec le r5r package (« ",
                      "itinéraires rapides et réalistes ») dans R. <p>Les calc",
                      "uls prennent en considération les infrastructures piéto",
                      "nnes et cyclistes ainsi que l'élévation.<p>Les coûts de",
                      " virage et de traversée d'une intersection ne sont pas ",
                      "considérés.")) |> 
  add_row(en = paste0("Open data from the City of Montreal, DMTI data, and Ope",
                      "nStreetMap data"), 
          fr = paste0("Données ouvertes de la Ville de Montréal, des données D",
                      "MTI et du réseau de rues OpenStreetMap.")) |> 
  
  add_row(en = paste0("Données Québec and DMTI"), 
          fr = paste0("Données Québec et DMTI")) |> 
  add_row(en = paste0("Travel time matrices in this module have been calculated with the",
                      "'r5r' R package (with the use of an Open Street Map network file ",
                      "and a GTFS public transport feed). Locations have been extracted ",
                      "from DMTI and Données Québec."), 
          fr = paste0("Les matrices de temps de parcours de ce module ont été ",
                      "calculées avec le package R 'r5r' (avec l'utilisation d",
                      "'un fichier réseau Open Street Map et d'un flux de tran",
                      "sport public GTFS). Les localisations ont été extraites",
                      " de DMTI et de Données Québec.")) |> 
  
  add_row(en = paste0("<p>Data made available by the firm Habitat. For more in",
                      "formation on the methods and data used for this module,",
                      " see <a href = 'https://fr.davidsuzuki.org/publication-",
                      "scientifique/le-role-des-infrastructures-natrelles-dans",
                      "-la-prevention-des-inondations-dans-la-communaute-metro",
                      "politaine-de-montreal/'>Maure et al., 2018, Le rôle des",
                      " infrastructures naturelles dans la prévention des inon",
                      "dations dans la Communauté métropolitaine de Montréal, ",
                      "Fondation David Suzuki.</a></p>"), 
          fr = paste0("<p>Données mises à disposition par l'entreprise Habitat",
                      ". Pour plus d'informations sur les méthodes et les donn",
                      "ées utilisées pour ce module, voir <a href = 'https://f",
                      "r.davidsuzuki.org/publication-scientifique/le-role-des-",
                      "infrastructures-natrelles-dans-la-prevention-des-inonda",
                      "tions-dans-la-communaute-metropolitaine-de-montreal/'>M",
                      "aure et al., 2018, Le rôle des infrastructures naturell",
                      "es dans la prévention des inondations dans la Communaut",
                      "é métropolitaine de Montréal, Fondation David Suzuki.</",
                      "a></p>")) |> 
  add_row(en = paste0("<p>For more information on how accessibility metrics ar",
                      "e calculated, see <a href = 'https://conservancy.umn.ed",
                      "u/bitstream/handle/11299/199892/CTS13-20_Access-Across-",
                      "America.pdf'>'Access Across America'</a>.</p>"), 
          fr = paste0("<p>Pour plus d'informations sur la façon dont les mesur",
                      "es d'accessibilité sont calculées, voir <a href = 'http",
                      "s://conservancy.umn.edu/bitstream/handle/11299/199892/C",
                      "TS13-20_Access-Across-America.pdf'>'Access Across Ameri",
                      "ca'</a>.</p>")) |> 
  add_row(en = paste0("<p><a href = 'https://nancyrossresearchgroup.ca/researc",
                      "h/can-ale/'>The Canadian Active Living Environments (Ca",
                      "n-ALE)</a> dataset is a geographic-based set of measure",
                      "s characterizing the active living environments (often ",
                      "referred to as 'walkability') of Canadian communities. ",
                      "The data is provided at the dissemination area level.</",
                      "p>"), 
          fr = paste0("<p><a href = 'https://nancyrossresearchgroup.ca/researc",
                      "h/can-ale/'>L'accessibilité à la vie active dans les mi",
                      "lieux de vie au Canada (AVA-Can)</a>  s'agit d'un ensem",
                      "ble de mesures géographiques caractérisant les milieux ",
                      "de vie favorisant la vie active (souvent appelés 'march",
                      "abilité') des collectivités canadiennes. Les données so",
                      "nt fournies au niveau de l'aire de diffusion.</p>")) |> 
  add_row(en = paste0("<p><a href = 'https://donnees.montreal.ca/ville-de-mont",
                      "real/vulnerabilite-changements-climatiques'>The data in",
                      " this module are cartographic representations of the vu",
                      "lnerability analysis</a> developed as part of the Clima",
                      "te change adaptation plan for the agglomeration of Mont",
                      "réal 2015-2020 for the following climate hazards: heavy",
                      " rainfall, heat waves, destructive storms, droughts and",
                      " floods.</p>"), 
          fr = paste0("<p><a href = 'https://donnees.montreal.ca/ville-de-mont",
                      "real/vulnerabilite-changements-climatiques'>Les données",
                      " de ce module sont des représentations cartographiques ",
                      "de l'analyse de vulnérabilité</a> élaborée dans le cadr",
                      "e du Plan d'adaptation aux changements climatiques de l",
                      "'agglomération de Montréal 2015-2020 pour les aléas cli",
                      "matiques suivants : fortes pluies, canicules, tempêtes ",
                      "destructrices, sécheresses et inondations.</p>")) |> 
  add_row(en = paste0("<p>This module presents <a href = 'https://www.statcan.",
                      "gc.ca/en/census/census-engagement/about'>housing data f",
                      "rom the 1996 to 2016 Canadian Censuses</a></p>"), 
          fr = paste0("<p>Ce module présente <a href = 'https://www.statcan.gc",
                      ".ca/fr/recensement/sensibilisation-recensement/a-propos",
                      "'>les données sur le logement provenant des recensement",
                      "s canadiens de 1996 à 2016</a></p>")) |> 
  add_row(en = paste0("<p>The census data (2016) in this module comes from c",
                      "ustom tabulations ordered by Centraide of Greater Montr",
                      "eal to Statistics Canada.</p>"), 
          fr = paste0("<p>Les données du recensement (2016) dans ce module pro",
                      "viennent de totalisations personnalisées commandées par",
                      " Centraide du Grand Montréal à Statistique Canada.</p>")) |> 
  add_row(en = paste0("About the data"), 
          fr = paste0("À propos des données")) |> 
  add_row(en = paste0("The data is spatially organized at the {data_organizati",
                      "on} scale."), 
          fr = paste0("Les données sont organisées spatialement à l'échelle '{",
                      "data_organization}'.")) |> 
  add_row(en = paste0("About main variable"), 
          fr = paste0("A propos de la variable principale")) |> 
  add_row(en = paste0("About compared variable"), 
          fr = paste0("A propos de la variable comparée")) |> 
  add_row(en = paste0("Dismiss"), 
          fr = paste0("Fermer")) |> 
  add_row(en = paste0("Download .csv"), 
          fr = paste0("Télécharger .csv")) |> 
  add_row(en = paste0("Download .shp"), 
          fr = paste0("Télécharger .shp")) |> 
  add_row(en = paste0("raster"), 
          fr = paste0("matricielle")) |> 
  add_row(en = paste0("for the year {time}"), 
          fr = paste0("pour l'année {time}")) |> 
  add_row(en = paste0("for the years {time[1]} and {time[2]}"), 
          fr = paste0("pour les années {time[1]} et {time[2]}")) |> 
  add_row(en = paste0("We do not have permission to make the variable <b>'{vari",
                      "ables_row$var_title}'</b> available for public download."), 
          fr = paste0("Nous n'avons pas la permission de rendre la variable <b",
                      ">'{variables_row$var_title}'</b> disponible pour un tél",
                      "échargement public.")) |> 
  add_row(en = paste0("The column `<b>{paste0(export_data[[var]], collapse = '",
                      "</b>` and `<b>')}</b>` contains data on {variables_row$",
                      "explanation} ('{variables_row$var_title}'){time_text}."), 
          fr = paste0("La colonne `<b>{paste0(export_data[[var]], collapse = '",
                      "</b>` and `<b>')}</b>` contient des données sur {variab",
                      "les_row$explanation} ('{variables_row$var_title}'){time",
                      "_text}.")) |> 
  add_row(en = paste0("The columns `<b>{paste0(export_data[[var]], collapse = ",
                      "'</b>` and `<b>')}</b>` contain data on {variables_row$",
                      "explanation} ('{variables_row$var_title}'){time_text}."), 
          fr = paste0("Les colonnes `<b>{paste0(export_data[[var]], collapse =",
                      " '</b>` et `<b>')}</b>` contiennent des données sur {va",
                      "riables_row$explanation} ('{variables_row$var_title}'){",
                      "time_text}.")) |> 
  add_row(en = paste0("The variable `<b>{variables_row$var_title}</b>` contain",
                      "(s) data on {variables_row$explanation} ('{variables_ro",
                      "w$var_title}'){time_text}."), 
          fr = paste0("La variable `<b>{variables_row$var_title}</b>` contient",
                      " des données sur {variables_row$explanation} ('{variabl",
                      "es_row$var_title}'){time_text}.")) |> 
  add_row(en = paste0("Values range from <b>{quant_info$min}</b> to <b>{quant_",
                      "info$max}</b>, with a mean of <b>{quant_info$mean}</b> ",
                      "and a standard deviation is <b>{quant_info$sd}</b>."), 
          fr = paste0("Les valeurs vont de <b>{quant_info$min}</b> à <b>{quant",
                      "_info$max}</b>, avec une moyenne de <b>{quant_info$mean",
                      "}</b> et un écart-type de <b>{quant_info$sd}</b>.")) |> 
  add_row(en = paste0("The numerator of the percentage is {vector_definition},",
                      " and the denominator is {parent_vector_definition}."), 
          fr = paste0("Le numérateur du pourcentage est {vector_definition}, e",
                      "t le dénominateur est {parent_vector_definition}.")) |> 
  add_row(en = paste0("The numerator of the percentage is {vector_definition},",
                      " and the summed denominators are {parent_vector_definit",
                      "ion}."), 
          fr = paste0("Le numérateur du pourcentage est {vector_definition}, e",
                      "t les dénominateurs additionnés sont {parent_vector_def",
                      "inition}.")) |> 
  add_row(en = paste0("The percentage has been done with the addition of the f",
                      "ollowing vectors, forming the numerator: {vector_defini",
                      "tion}. The denominator is {parent_vector_definition}."), 
          fr = paste0("Le pourcentage a été réalisé avec l'addition des vecteu",
                      "rs suivants, formant le numérateur : {vector_definition",
                      "}. Le dénominateur est {parent_vector_definition}.")) |> 
  add_row(en = paste0("The percentage has been done with the addition of the f",
                      "ollowing vectors, forming the numerator: {vector_defini",
                      "tion}. The summed denominators are {parent_vector_defin",
                      "ition}."), 
          fr = paste0("Le pourcentage a été réalisé avec l'addition des vecteu",
                      "rs suivants, formant le numérateur : {vector_definition",
                      "}. Les dénominateurs additionnés sont {parent_vector_de",
                      "finition}.")) |> 
  add_row(en = paste0("The Canadian Census vector is {vector_definition}. It i",
                      "s the average of {parent_vector_definition}."), 
          fr = paste0("Le vecteur du recensement canadien est {vector_definiti",
                      "on}. Il s'agit de la moyenne de {parent_vector_definiti",
                      "on}.")) |> 
  add_row(en = paste0("The Canadian Census vector is {vector_definition}. It i",
                      "s the median of {parent_vector_definition}."), 
          fr = paste0("Le vecteur du recensement canadien est {vector_definiti",
                      "on}. Il s'agit de la médiane de {parent_vector_definiti",
                      "on}.")) |> 
  
  add_row(en = paste0("<a href = 'https://www.canada.ca/en/public-health/servi",
                      "ces/reports-publications/health-promotion-chronic-disea",
                      "se-prevention-canada-research-policy-practice/vol-40-no",
                      "-9-2020/canbics-classification-system-naming-convention",
                      "-cycling-infrastructure.html'>Can-BICS, or Canadian Bik",
                      "eway Comfort and Safety,</a> is a classification system",
                      " for cycling infrastructure in Canada. This system is b",
                      "ased on three tiers that considers safety and user comf",
                      "ort: high-comfort bikeways, medium-comfort bikeways, an",
                      "d low-comfort bikeways."), 
          fr = paste0("<a target = '_blank' href = 'https://www.canada.ca/fr/s",
                      "ante-publique/services/rapports-publications/promotion-",
                      "sante-prevention-maladies-chroniques-canada-recherche-p",
                      "olitiques-pratiques/vol-40-no-9-2020/systeme-classifica",
                      "tion-canbics-convention-appellation-amenagements-cyclab",
                      "les.html'>Can-BICS, ou système de classification du con",
                      "fort et de la sécurité des voies cyclables canadiennes,",
                      "</a> est un système de classification des infrastructur",
                      "es cyclables au Canada. Ce système est basé sur trois n",
                      "iveaux qui prennent en compte la sécurité et le confort",
                      " des usagers : les voies cyclables à confort élevé, les",
                      " voies cyclables à confort moyen et les voies cyclables",
                      " à faible confort.")) |> 
  
  
  add_row(en = paste0("{time[1]} and {time[2]}"), 
          fr = paste0("{time[1]} et {time[2]}")) |> 
  add_row(en = paste0("For {time}: {census_details}"), 
          fr = paste0("Pour {time}: {census_details}")) |> 
  add_row(en = paste0("The data comes from the {time_text} Canadian Census. {c",
                      "ensus_details}"), 
          fr = paste0("Les données proviennent du recensement canadien de {tim",
                      "e_text}. {census_details}")) |> 
  add_row(en = paste0("The data comes from the {time_text} Canadian Censuses. ",
                      "{census_details}"), 
          fr = paste0("Les données proviennent des recensements canadiens de {",
                      "time_text}. {census_details}")) |> 
  add_row(en = paste0("The data comes from {source}."), 
          fr = paste0("Les données proviennent de '{source}'.")) |> 
  add_row(en = paste0("For the City of Montreal's boroughs, `{variables_row$va",
                      "r_title}` is spatially interpolated from {from}s."), 
          fr = paste0("Pour les arrondissements de la ville de Montréal, `{var",
                      "iables_row$var_title}` est interpolé spatialement à par",
                      "tir '{from}'.")) |> 
  add_row(en = paste0("`{variables_row$var_title}` at the {df} scale is spatia",
                      "lly interpolated from {from}s."), 
          fr = paste0("`{variables_row$var_title}` à l'échelle '{df}' est spat",
                      "ialement interpolé à partir {from}.")) |> 
  add_row(en = paste0("The data is represented as {df}s, but the underlying da",
                      "taset is spatially organised as {data_origin}s."), 
          fr = paste0("Les données sont représentés à l'/au '{df}', mais l'ens",
                      "emble des données sous-jacentes est organisé spatialeme",
                      "nt comme '{data_origin}'.")) |> 
  add_row(en = paste0("Export data"), 
          fr = paste0("Exportation des données")) |> 
  add_row(en = paste0("Data preview (first 10 rows)"), 
          fr = paste0("Aperçu des données (10 premières lignes)")) |> 
  # All sources
  add_row(en = paste0("Canadian census"), 
          fr = paste0("Recensement canadien")) |> 
  add_row(en = paste0("McGill Geo-Social Determinants of Health Research Group"), 
          fr = paste0("Groupe de recherche sur les déterminants géosociaux de la santé de McGill")) |> 
  add_row(en = paste0("City of Montreal's open data website"), 
          fr = paste0("Site de données ouvertes de la ville de Montréal")) |> 
  add_row(en = paste0("Curbcut team"), 
          fr = paste0("L'équipe Curbcut")) |> 
  add_row(en = paste0("David Suzuki Foundation"), 
          fr = paste0("Fondation David Suzuki")) |> 
  add_row(en = paste0("Centraide of Greater Montreal"), 
          fr = paste0("Centraide du Grand Montréal")) |> 
  add_row(en = paste0("Canada Mortgage and Housing Corporation"), 
          fr = paste0("Société canadienne d'hypothèques et de logement")) |> 
  add_row(en = paste0("Job and population data from Statistics Canada. Travel time calculations from OSM and GTFS."), 
          fr = paste0("Données sur les emplois et la population provenant de Statistique Canada. Calculs des temps de trajet à partir d'OSM et de GTFS.")) |> 
  add_row(en = paste0("<p>The vacancy rate data in this module comes from the Canada Mortgage and Housing Corporation.</p>"), 
          fr = paste0("<p>Les données sur le taux d'inoccupation dans ce module proviennent de la Société canadienne d'hypothèques et de logement.</p>")) |> 
  add_row(en = paste0("Meghan Winters at Faculty of Health Sciences, Simon Fraser University"), 
          fr = paste0("Meghan Winters à la Faculté des sciences de la santé, Université Simon Fraser")) |> 
  
  
# Auto translated a few vectors -------------------------------------------
# 
# census_variables <- qread("data/census_variables.qs")
# translate_fun <- function(x) deeplr::toFrench2(x, auth_key = .deepl_key)
# 
# en <- census_variables$vec_label |> 
#   unlist() |> 
#   unique() |> 
#   na.omit()
# 
# deepl_translated <- 
#   tibble(en = en,
#          fr = map_chr(en, translate_fun))
# 
# form_translation_tibble(deepl_translated)

  add_row(en = paste0("Rented"), 
          fr = paste0("Loué")) |> 
  add_row(en = paste0("Average gross rent $"), 
          fr = paste0("Loyer brut moyen")) |> 
  add_row(en = paste0("Major repairs"), 
          fr = paste0("Réparations majeures")) |> 
  add_row(en = paste0("Average value of dwelling $"), 
          fr = paste0("Valeur moyenne du logement")) |> 
  add_row(en = paste0("Movers"), 
          fr = paste0("Déménageurs")) |> 
  add_row(en = paste0("Single-detached house"), 
          fr = paste0("Maison individuelle")) |> 
  add_row(en = paste0("Median household income $"), 
          fr = paste0("Revenu médian des ménages")) |> 
  add_row(en = paste0("Under $10,000"), 
          fr = paste0("Moins de 10 000 $.")) |> 
  add_row(en = paste0("$  10,000 - $19,999"), 
          fr = paste0("$ 10,000 - $19,999")) |> 
  add_row(en = paste0("$  20,000 - $29,999"), 
          fr = paste0("$ 20,000 - $29,999")) |> 
  add_row(en = paste0("$  30,000 - $39,999"), 
          fr = paste0("$ 30,000 - $39,999")) |> 
  add_row(en = paste0("$  40,000 - $49,999"), 
          fr = paste0("$ 40,000 - $49,999")) |> 
  add_row(en = paste0("$  50,000 - $59,999"), 
          fr = paste0("$ 50,000 - $59,999")) |> 
  add_row(en = paste0("$  60,000 - $69,999"), 
          fr = paste0("$ 60,000 - $69,999")) |> 
  add_row(en = paste0("$  70,000 - $79,999"), 
          fr = paste0("$ 70,000 - $79,999")) |> 
  add_row(en = paste0("$  80,000 - $89,999"), 
          fr = paste0("$ 80,000 - $89,999")) |> 
  add_row(en = paste0("$  90,000 - $99,999"), 
          fr = paste0("$ 90,000 - $99,999")) |> 
  add_row(en = paste0("$100,000 and over"), 
          fr = paste0("100 000 $ et plus")) |> 
  add_row(en = paste0("Total immigrants by selected countries of birth"), 
          fr = paste0("Total des immigrants par pays de naissance sélectionnés",
                      "")) |> 
  add_row(en = paste0("1991-1996, period of immigration"), 
          fr = paste0("1991-1996, période d'immigration")) |> 
  add_row(en = paste0("Total visible minority population"), 
          fr = paste0("Population totale des minorités visibles")) |> 
  add_row(en = paste0("Total Aboriginal population"), 
          fr = paste0("Population autochtone totale")) |> 
  add_row(en = paste0("Car, truck, van as driver"), 
          fr = paste0("Voiture, camion, camionnette en tant que conducteur")) |> 
  add_row(en = paste0("Car, truck, van as passenger"), 
          fr = paste0("Voiture, camion, camionnette en tant que passager")) |> 
  add_row(en = paste0("Walked to work"), 
          fr = paste0("J'ai marché jusqu'au travail")) |> 
  add_row(en = paste0("Bicycle"), 
          fr = paste0("Bicyclette")) |> 
  add_row(en = paste0("Public transit"), 
          fr = paste0("Transport public")) |> 
  add_row(en = paste0("Living alone"), 
          fr = paste0("Vivre seul")) |> 
  add_row(en = paste0("French only"), 
          fr = paste0("Français seulement")) |> 
  add_row(en = paste0("English only"), 
          fr = paste0("Anglais seulement")) |> 
  add_row(en = paste0("English and French"), 
          fr = paste0("Anglais et français")) |> 
  add_row(en = paste0("Neither English nor French"), 
          fr = paste0("Ni anglais ni français")) |> 
  add_row(en = paste0("0-4"), 
          fr = paste0("0-4")) |> 
  add_row(en = paste0("5-9"), 
          fr = paste0("5-9")) |> 
  add_row(en = paste0("10-14"), 
          fr = paste0("10-14")) |> 
  add_row(en = paste0("15-19"), 
          fr = paste0("15-19")) |> 
  add_row(en = paste0("20-24"), 
          fr = paste0("20-24")) |> 
  add_row(en = paste0("25-29"), 
          fr = paste0("25-29")) |> 
  add_row(en = paste0("30-34"), 
          fr = paste0("30-34")) |> 
  add_row(en = paste0("35-39"), 
          fr = paste0("35-39")) |> 
  add_row(en = paste0("40-44"), 
          fr = paste0("40-44")) |> 
  add_row(en = paste0("45-49"), 
          fr = paste0("45-49")) |> 
  add_row(en = paste0("50-54"), 
          fr = paste0("50-54")) |> 
  add_row(en = paste0("55-59"), 
          fr = paste0("55-59")) |> 
  add_row(en = paste0("60-64"), 
          fr = paste0("60-64")) |> 
  add_row(en = paste0("65-69"), 
          fr = paste0("65-69")) |> 
  add_row(en = paste0("70-74"), 
          fr = paste0("70-74")) |> 
  add_row(en = paste0("75-79"), 
          fr = paste0("75-79")) |> 
  add_row(en = paste0("80-84"), 
          fr = paste0("80-84")) |> 
  add_row(en = paste0("85+"), 
          fr = paste0("85+")) |> 
  add_row(en = paste0("With bachelor's degree or higher"), 
          fr = paste0("Titulaires d'une licence ou d'un diplôme supérieur")) |> 
  add_row(en = paste0("Without secondary school graduation certificate"), 
          fr = paste0("Sans certificat de fin d'études secondaires")) |> 
  add_row(en = paste0("$10,000 - $19,999"), 
          fr = paste0("$10,000 - $19,999")) |> 
  add_row(en = paste0("$20,000 - $29,999"), 
          fr = paste0("$20,000 - $29,999")) |> 
  add_row(en = paste0("$30,000 - $39,999"), 
          fr = paste0("$30,000 - $39,999")) |> 
  add_row(en = paste0("$40,000 - $49,999"), 
          fr = paste0("$40,000 - $49,999")) |> 
  add_row(en = paste0("$50,000 - $59,999"), 
          fr = paste0("$50,000 - $59,999")) |> 
  add_row(en = paste0("$60,000 - $69,999"), 
          fr = paste0("$60,000 - $69,999")) |> 
  add_row(en = paste0("$70,000 - $79,999"), 
          fr = paste0("$70,000 - $79,999")) |> 
  add_row(en = paste0("$80,000 - $89,999"), 
          fr = paste0("$80,000 - $89,999")) |> 
  add_row(en = paste0("$90,000 - $99,999"), 
          fr = paste0("$90,000 - $99,999")) |> 
  add_row(en = paste0("Total immigrants by selected places of birth"), 
          fr = paste0("Total des immigrants par lieux de naissance sélectionné",
                      "s")) |> 
  add_row(en = paste0("1996-2001"), 
          fr = paste0("1996-2001")) |> 
  add_row(en = paste0("Total Aboriginal identity population"), 
          fr = paste0("Population totale ayant une identité autochtone")) |> 
  add_row(en = paste0("Car, truck, van, as driver"), 
          fr = paste0("Voiture, camion, camionnette, en tant que conducteur")) |> 
  add_row(en = paste0("Car, truck, van, as passenger"), 
          fr = paste0("Voiture, camion, camionnette, en tant que passager")) |> 
  add_row(en = paste0("Walked"), 
          fr = paste0("Marche")) |> 
  add_row(en = paste0("54 Professional, scientific and technical services"), 
          fr = paste0("54 Services professionnels, scientifiques et techniques",
                      "")) |> 
  add_row(en = paste0("55 Management of companies and enterprises"), 
          fr = paste0("55 Gestion de sociétés et d'entreprises")) |> 
  add_row(en = paste0("51 Information and cultural industries"), 
          fr = paste0("51 Industries de l'information et de la culture")) |> 
  add_row(en = paste0("71 Arts, entertainment and recreation"), 
          fr = paste0("71 Arts, spectacles et loisirs")) |> 
  add_row(en = paste0("With children at home"), 
          fr = paste0("Avec des enfants à la maison")) |> 
  add_row(en = paste0("Total lone-parent families by sex of parent and number ",
                      "of children"), 
          fr = paste0("Total des familles monoparentales selon le sexe du pare",
                      "nt et le nombre d'enfants")) |> 
  add_row(en = paste0("Without high school graduation certificate"), 
          fr = paste0("Sans certificat de fin d'études secondaires")) |> 
  add_row(en = paste0("$10,000 to $19,999"), 
          fr = paste0("De 10 000 $ à 19 999 $.")) |> 
  add_row(en = paste0("$20,000 to $29,999"), 
          fr = paste0("De 20 000 $ à 29 999 $.")) |> 
  add_row(en = paste0("$30,000 to $39,999"), 
          fr = paste0("De 30 000 $ à 39 999 $.")) |> 
  add_row(en = paste0("$40,000 to $49,999"), 
          fr = paste0("De 40 000 $ à 49 999 $.")) |> 
  add_row(en = paste0("$50,000 to $59,999"), 
          fr = paste0("De 50 000 $ à 59 999 $.")) |> 
  add_row(en = paste0("$60,000 to $69,999"), 
          fr = paste0("De 60 000 $ à 69 999 $.")) |> 
  add_row(en = paste0("$70,000 to $79,999"), 
          fr = paste0("De 70 000 $ à 79 999 $.")) |> 
  add_row(en = paste0("$80,000 to $89,999"), 
          fr = paste0("De 80 000 $ à 89 999 $.")) |> 
  add_row(en = paste0("$90,000 to $99,999"), 
          fr = paste0("De 90 000 $ à 99 999 $.")) |> 
  add_row(en = paste0("Immigrants"), 
          fr = paste0("Immigrants")) |> 
  add_row(en = paste0("2001 to 2006"), 
          fr = paste0("2001 à 2006")) |> 
  add_row(en = paste0("0 to 4 years"), 
          fr = paste0("0 à 4 ans")) |> 
  add_row(en = paste0("5 to 9 years"), 
          fr = paste0("5 à 9 ans")) |> 
  add_row(en = paste0("10 to 14 years"), 
          fr = paste0("10 à 14 ans")) |> 
  add_row(en = paste0("15 to 19 years"), 
          fr = paste0("15 à 19 ans")) |> 
  add_row(en = paste0("20 to 24 years"), 
          fr = paste0("20 à 24 ans")) |> 
  add_row(en = paste0("25 to 29 years"), 
          fr = paste0("25 à 29 ans")) |> 
  add_row(en = paste0("30 to 34 years"), 
          fr = paste0("30 à 34 ans")) |> 
  add_row(en = paste0("35 to 39 years"), 
          fr = paste0("35 à 39 ans")) |> 
  add_row(en = paste0("40 to 44 years"), 
          fr = paste0("40 à 44 ans")) |> 
  add_row(en = paste0("45 to 49 years"), 
          fr = paste0("45 à 49 ans")) |> 
  add_row(en = paste0("50 to 54 years"), 
          fr = paste0("50 à 54 ans")) |> 
  add_row(en = paste0("55 to 59 years"), 
          fr = paste0("55 à 59 ans")) |> 
  add_row(en = paste0("60 to 64 years"), 
          fr = paste0("60 à 64 ans")) |> 
  add_row(en = paste0("65 to 69 years"), 
          fr = paste0("65 à 69 ans")) |> 
  add_row(en = paste0("70 to 74 years"), 
          fr = paste0("70 à 74 ans")) |> 
  add_row(en = paste0("75 to 79 years"), 
          fr = paste0("75 à 79 ans")) |> 
  add_row(en = paste0("80 to 84 years"), 
          fr = paste0("80 à 84 ans")) |> 
  add_row(en = paste0("85 years and over"), 
          fr = paste0("85 ans et plus")) |> 
  add_row(en = paste0("University certificate, diploma or degree"), 
          fr = paste0("Certificat, diplôme ou grade universitaire")) |> 
  add_row(en = paste0("No certificate, diploma or degree"), 
          fr = paste0("Aucun certificat, diplôme ou grade")) |> 
  add_row(en = paste0("Renter"), 
          fr = paste0("Locataire")) |> 
  add_row(en = paste0("Average monthly shelter costs for rented dwellings ($)"), 
          fr = paste0("Frais de logement mensuels moyens pour les logements lo",
                      "ués ($)")) |> 
  add_row(en = paste0("Major repairs needed"), 
          fr = paste0("Réparations majeures nécessaires")) |> 
  add_row(en = paste0("Average value of dwellings ($)"), 
          fr = paste0("Valeur moyenne des logements ($)")) |> 
  add_row(en = paste0("Not suitable"), 
          fr = paste0("Ne convient pas")) |> 
  add_row(en = paste0("% of tenant households spending 30% or more of househol",
                      "d total income on shelter costs"), 
          fr = paste0("Pourcentage des ménages locataires qui consacrent 30 % ",
                      "ou plus de leur revenu total aux frais de logement.")) |> 
  add_row(en = paste0("% of owner households spending 30% or more of household",
                      " total income on shelter costs"), 
          fr = paste0("Pourcentage de ménages propriétaires consacrant 30 % ou",
                      " plus du revenu total du ménage aux frais de logement")) |> 
  add_row(en = paste0("Median household total income $"), 
          fr = paste0("Revenu total du ménage médian $.")) |> 
  add_row(en = paste0("Under $5,000"), 
          fr = paste0("Moins de 5 000 $.")) |> 
  add_row(en = paste0("$5,000 to $9,999"), 
          fr = paste0("De 5 000 à 9 999 dollars")) |> 
  add_row(en = paste0("$10,000 to $14,999"), 
          fr = paste0("De 10 000 $ à 14 999 $.")) |> 
  add_row(en = paste0("$15,000 to $19,999"), 
          fr = paste0("De 15 000 à 19 999 dollars")) |> 
  add_row(en = paste0("$60,000 to $79,999"), 
          fr = paste0("De 60 000 $ à 79 999 $.")) |> 
  add_row(en = paste0("$80,000 to $99,999"), 
          fr = paste0("De 80 000 $ à 99 999 $.")) |> 
  add_row(en = paste0("$100,000 to $124,999"), 
          fr = paste0("100 000 $ à 124 999 $.")) |> 
  add_row(en = paste0("$125,000 to $149,999"), 
          fr = paste0("De 125 000 $ à 149 999 $.")) |> 
  add_row(en = paste0("$150,000 and over"), 
          fr = paste0("150 000 $ et plus")) |> 
  add_row(en = paste0("2006 to 2011"), 
          fr = paste0("2006 à 2011")) |> 
  add_row(en = paste0("Aboriginal identity"), 
          fr = paste0("Identité autochtone")) |> 
  add_row(en = paste0("Car, truck or van - as a driver"), 
          fr = paste0("Voiture, camion ou camionnette - en tant que conducteur",
                      "")) |> 
  add_row(en = paste0("Car, truck or van - as a passenger"), 
          fr = paste0("Voiture, camion ou camionnette - en tant que passager")) |> 
  add_row(en = paste0("University certificate, diploma or degree at bachelor l",
                      "evel or above"), 
          fr = paste0("Certificat, diplôme ou grade universitaire de niveau li",
                      "cence ou supérieur")) |> 
  add_row(en = paste0("Spending 30% or more of income on shelter costs"), 
          fr = paste0("Consacrer 30 % ou plus de ses revenus aux frais de loge",
                      "ment")) |> 
  add_row(en = paste0("% of tenant households spending 30% or more of its inco",
                      "me on shelter costs"), 
          fr = paste0("Pourcentage de ménages locataires consacrant 30 % ou pl",
                      "us de leurs revenus aux frais de logement.")) |> 
  add_row(en = paste0("% of owner households spending 30% or more of its incom",
                      "e on shelter costs"), 
          fr = paste0("Pourcentage de ménages propriétaires consacrant 30 % ou",
                      " plus de leurs revenus aux frais de logement.")) |> 
  add_row(en = paste0("Median total income of households in 2015 ($)"), 
          fr = paste0("Revenu total médian des ménages en 2015 ($)")) |> 
  add_row(en = paste0("$20,000 to $24,999"), 
          fr = paste0("20 000 à 24 999 dollars")) |> 
  add_row(en = paste0("$25,000 to $29,999"), 
          fr = paste0("De 25 000 $ à 29 999 $.")) |> 
  add_row(en = paste0("$30,000 to $34,999"), 
          fr = paste0("De 30 000 $ à 34 999 $.")) |> 
  add_row(en = paste0("$35,000 to $39,999"), 
          fr = paste0("De 35 000 $ à 39 999 $.")) |> 
  add_row(en = paste0("$40,000 to $44,999"), 
          fr = paste0("De 40 000 $ à 44 999 $.")) |> 
  add_row(en = paste0("$45,000 to $49,999"), 
          fr = paste0("De 45 000 $ à 49 999 $.")) |> 
  add_row(en = paste0("Prevalence of low income based on the Low-income measur",
                      "e, after tax (LIM-AT) (%)"), 
          fr = paste0("Prévalence de bas revenus basée sur la mesure de bas re",
                      "venus, après impôts (LIM-AT) (%)")) |> 
  add_row(en = paste0("2011 to 2016"), 
          fr = paste0("2011 à 2016")) |> 
  add_row(en = paste0("Car, truck, van - as a driver"), 
          fr = paste0("Voiture, camion, camionnette - en tant que conducteur")) |> 
  add_row(en = paste0("Car, truck, van - as a passenger"), 
          fr = paste0("Voiture, camion, camionnette - en tant que passager")) |> 
  add_row(en = paste0("Less than 15 minutes"), 
          fr = paste0("Moins de 15 minutes")) |> 
  add_row(en = paste0("15 to 29 minutes"), 
          fr = paste0("15 à 29 minutes")) |> 
  add_row(en = paste0("30 to 44 minutes"), 
          fr = paste0("30 à 44 minutes")) |> 
  add_row(en = paste0("45 to 59 minutes"), 
          fr = paste0("45 à 59 minutes")) |> 
  add_row(en = paste0("60 minutes and over"), 
          fr = paste0("60 minutes et plus")) |> 
  add_row(en = paste0("With children in a census family"), 
          fr = paste0("Avec des enfants dans une famille de recensement")) |> 
  add_row(en = paste0("One-person households"), 
          fr = paste0("Ménages d'une personne")) |> 
  add_row(en = paste0("0 to 14 years"), 
          fr = paste0("De 0 à 14 ans")) |> 
  add_row(en = paste0("15 to 64 years"), 
          fr = paste0("15 à 64 ans")) |> 
  add_row(en = paste0("65 years and over"), 
          fr = paste0("65 ans et plus")) |> 

# # Auto translated a few vectors -------------------------------------------
# en <- census_variables$parent_vec_label |>
#   unlist() |>
#   unique() |>
#   na.omit()
# 
# deepl_translated <-
#   tibble(en = en,
#          fr = map_chr(en, translate_fun))
# 
# form_translation_tibble(deepl_translated)

add_row(en = paste0("Total number of occupied private dwellings"), 
        fr = paste0("Nombre total de logements privés occupés")) |> 
  add_row(en = paste0("Rented"), 
          fr = paste0("Loué")) |> 
  add_row(en = paste0("Owned"), 
          fr = paste0("Propriété de")) |> 
  add_row(en = paste0("Total by mobility status 1 year ago"), 
          fr = paste0("Total par statut de mobilité Il y a 1 an")) |> 
  add_row(en = paste0("Total by mobility status 5 years ago"), 
          fr = paste0("Total par statut de mobilité Il y a 5 ans")) |> 
  add_row(en = paste0("Total number of occupied private dwellings by structura",
                      "l type of dwelling"), 
          fr = paste0("Nombre total de logements privés occupés par type struc",
                      "turel de logement")) |> 
  add_row(en = paste0("Household income of all private households"), 
          fr = paste0("Revenu de l'ensemble des ménages privés")) |> 
  add_row(en = paste0("Total population by place of birth"), 
          fr = paste0("Population totale par lieu de naissance")) |> 
  add_row(en = paste0("Total - Total population by visible minority population",
                      ""), 
          fr = paste0("Total - Population totale par minorité visible")) |> 
  add_row(en = paste0("Total population by Aboriginal groups and non-Aborigina",
                      "l population"), 
          fr = paste0("Population totale par groupes autochtones et population",
                      " non autochtone")) |> 
  add_row(en = paste0("Total employed labour force 15 years and over by mode o",
                      "f transportation"), 
          fr = paste0("Population active occupée totale de 15 ans et plus par ",
                      "mode de transport")) |> 
  add_row(en = paste0("Number of non-family persons"), 
          fr = paste0("Nombre de personnes non familiales")) |> 
  add_row(en = paste0("Total population by knowledge of official languages"), 
          fr = paste0("Population totale selon la connaissance des langues off",
                      "icielles")) |> 
  add_row(en = paste0("Male, total"), 
          fr = paste0("Male, total")) |> 
  add_row(en = paste0("Female, total"), 
          fr = paste0("Femme, total")) |> 
  add_row(en = paste0("Total population 15 years and over by highest level of ",
                      "schooling"), 
          fr = paste0("Population totale de 15 ans et plus par niveau d'études",
                      " le plus élevé")) |> 
  add_row(en = paste0("Total number of occupied private dwellings by rooms"), 
          fr = paste0("Nombre total de logements privés occupés par pièces")) |> 
  add_row(en = paste0("Tenant households in non-farm, non-reserve private dwel",
                      "lings"), 
          fr = paste0("Ménages locataires de logements privés non agricoles, h",
                      "ors réserve")) |> 
  add_row(en = paste0("Owner households in non-farm, non-reserve private dwell",
                      "ings"), 
          fr = paste0("Ménages propriétaires de logements privés non agricoles",
                      ", hors réserve")) |> 
  add_row(en = paste0("Total population 1 year and over by mobility status 1 y",
                      "ear ago"), 
          fr = paste0("Population totale de 1 an et plus par statut de mobilit",
                      "é il y a 1 an")) |> 
  add_row(en = paste0("Total population 5 years and over by mobility status 5 ",
                      "years ago"), 
          fr = paste0("Population totale de 5 ans et plus par statut de mobili",
                      "té il y a 5 ans")) |> 
  add_row(en = paste0("Household income in 2000 of all private households"), 
          fr = paste0("Revenu des ménages en 2000 de tous les ménages privés")) |> 
  add_row(en = paste0("Total population by immigrant status and place of birth",
                      ""), 
          fr = paste0("Population totale par statut d'immigrant et lieu de nai",
                      "ssance")) |> 
  add_row(en = paste0("Total population by visible minority groups"), 
          fr = paste0("Population totale par groupes de minorités visibles")) |> 
  add_row(en = paste0("Total population by Aboriginal and non-Aboriginal popul",
                      "ation"), 
          fr = paste0("Population totale par population autochtone et non auto",
                      "chtone")) |> 
  add_row(en = paste0("All industries"), 
          fr = paste0("Tous les secteurs")) |> 
  add_row(en = paste0("Total number of census families in private households"), 
          fr = paste0("Nombre total de familles de recensement dans les ménage",
                      "s privés")) |> 
  add_row(en = paste0("Total population 20 years and over by highest level of ",
                      "schooling"), 
          fr = paste0("Population totale de 20 ans et plus par niveau d'études",
                      " le plus élevé")) |> 
  add_row(en = paste0("Total number of occupied private dwellings by housing t",
                      "enure"), 
          fr = paste0("Nombre total de logements privés occupés, par type de l",
                      "ogement")) |> 
  add_row(en = paste0("Tenant-occupied private non-farm, non-reserve dwellings",
                      ""), 
          fr = paste0("Logements privés non agricoles, hors réserve, occupés p",
                      "ar des locataires")) |> 
  add_row(en = paste0("Total number of occupied private dwellings by condition",
                      " of dwelling"), 
          fr = paste0("Nombre total de logements privés occupés selon la condi",
                      "tion du logement")) |> 
  add_row(en = paste0("Owner-occupied private non-farm, non-reserve dwellings"), 
          fr = paste0("Logements privés non agricoles, hors réserve, occupés p",
                      "ar leur propriétaire")) |> 
  add_row(en = paste0("Total - Mobility status 1 year ago"), 
          fr = paste0("Total - Statut de mobilité il y a 1 an")) |> 
  add_row(en = paste0("Total - Mobility status 5 years ago"), 
          fr = paste0("Total - Statut de mobilité il y a 5 ans")) |> 
  add_row(en = paste0("Household income in 2005 of private households"), 
          fr = paste0("Revenu des ménages privés en 2005")) |> 
  add_row(en = paste0("Total population by Aboriginal and non-Aboriginal ident",
                      "ity population"), 
          fr = paste0("Population totale par identité autochtone et non autoch",
                      "tone")) |> 
  add_row(en = paste0("Total employed labour force 15 years and over with usua",
                      "l place of work or no fixed workplace address by mode o",
                      "f transportation"), 
          fr = paste0("Population active totale de 15 ans et plus ayant un lie",
                      "u de travail habituel ou sans adresse de travail fixe, ",
                      "par mode de transport")) |> 
  add_row(en = paste0("Number of persons not in census families"), 
          fr = paste0("Nombre de personnes ne faisant pas partie des familles ",
                      "de recensement")) |> 
  add_row(en = paste0("Total population 15 to 24 years by highest certificate,",
                      " diploma or degree"), 
          fr = paste0("Population totale de 15 à 24 ans par certificat, diplôm",
                      "e ou grade le plus élevé")) |> 
  add_row(en = paste0("Total population 25 to 64 years by highest certificate,",
                      " diploma or degree"), 
          fr = paste0("Population totale de 25 à 64 ans par certificat, diplôm",
                      "e ou grade le plus élevé")) |> 
  add_row(en = paste0("Total population 65 years and over by highest certifica",
                      "te, diploma or degree"), 
          fr = paste0("Population totale de 65 ans et plus par certificat, dip",
                      "lôme ou grade le plus élevé")) |> 
  add_row(en = paste0("Total number of private households by tenure"), 
          fr = paste0("Nombre total de ménages privés par mode d'occupation")) |> 
  add_row(en = paste0("Number of tenant households in non-farm, non-reserve pr",
                      "ivate dwellings"), 
          fr = paste0("Nombre de ménages locataires dans des logements privés ",
                      "non agricoles et hors réserve")) |> 
  add_row(en = paste0("Number of owner households in non-farm, non-reserve pri",
                      "vate dwellings"), 
          fr = paste0("Nombre de ménages propriétaires dans des logements priv",
                      "és non agricoles, hors réserve")) |> 
  add_row(en = paste0("Total number of private households by housing suitabili",
                      "ty"), 
          fr = paste0("Nombre total de ménages privés par type de logement")) |> 
  add_row(en = paste0("Household income in 2010 of private households"), 
          fr = paste0("Revenu des ménages privés en 2010")) |> 
  add_row(en = paste0("Household total income in 2010 of private households"), 
          fr = paste0("Revenu total des ménages en 2010 des ménages privés")) |> 
  add_row(en = paste0("Total population in private households by immigrant sta",
                      "tus and period of immigration"), 
          fr = paste0("Population totale dans les ménages privés selon le stat",
                      "ut d'immigrant et la période d'immigration")) |> 
  add_row(en = paste0("Total population in private households by visible minor",
                      "ity"), 
          fr = paste0("Population totale dans les ménages privés par minorité ",
                      "visible")) |> 
  add_row(en = paste0("Total population in private households by Aboriginal id",
                      "entity"), 
          fr = paste0("Population totale dans les ménages privés selon l'ident",
                      "ité autochtone")) |> 
  add_row(en = paste0("Total employed population aged 15 years and over with a",
                      " usual place of work or no fixed workplace address by m",
                      "ode of transportation"), 
          fr = paste0("Population active totale âgée de 15 ans et plus ayant u",
                      "n lieu de travail habituel ou sans adresse de travail f",
                      "ixe, par mode de transport")) |> 
  add_row(en = paste0("Knowledge of official languages - Total population excl",
                      "uding institutional residents"), 
          fr = paste0("Connaissance des langues officielles - Population total",
                      "e, à l'exclusion des résidents en institution")) |> 
  add_row(en = paste0("Total population by age groups"), 
          fr = paste0("Population totale par groupes d'âge")) |> 
  add_row(en = paste0("Total population aged 15 years and over by highest cert",
                      "ificate, diploma or degree"), 
          fr = paste0("Population totale âgée de 15 ans et plus par certificat",
                      ", diplôme ou grade le plus élevé")) |> 
  add_row(en = paste0("Total - Private households by tenure - 25% sample data"), 
          fr = paste0("Total - Ménages privés par mode d'occupation - Données ",
                      "échantillon (25%)")) |> 
  add_row(en = paste0("Total - Tenant households in non-farm, non-reserve priv",
                      "ate dwellings - 25% sample data"), 
          fr = paste0("Total - Ménages locataires dans des logements privés no",
                      "n agricoles, hors réserve - Données-échantillon (25%)")) |> 
  add_row(en = paste0("Total - Occupied private dwellings by dwelling conditio",
                      "n - 25% sample data"), 
          fr = paste0("Total - Logements privés occupés selon l'état du logeme",
                      "nt - Données-échantillon (25%)")) |> 
  add_row(en = paste0("Total - Owner households in non-farm, non-reserve priva",
                      "te dwellings - 25% sample data"), 
          fr = paste0("Total - Ménages propriétaires de logements privés non a",
                      "gricoles, hors réserve - Données-échantillon (25%)")) |> 
  add_row(en = paste0("Total -  Owner and tenant households with household tot",
                      "al income greater than zero, in non-farm, non-reserve p",
                      "rivate dwellings by shelter-cost-to-income ratio - 25% ",
                      "sample data"), 
          fr = paste0("Total - Ménages propriétaires et locataires dont le rev",
                      "enu total du ménage est supérieur à zéro, dans des loge",
                      "ments privés non agricoles, hors réserve, selon le rapp",
                      "ort frais de logement/revenu - Données-échantillon (25%",
                      ")")) |> 
  add_row(en = paste0("Total - Private households by housing suitability - 25%",
                      " sample data"), 
          fr = paste0("Total - Ménages privés selon la taille du logement - Do",
                      "nnées-échantillon (25%)")) |> 
  add_row(en = paste0("Total - Mobility status 1 year ago - 25% sample data"), 
          fr = paste0("Total - Statut de mobilité il y a 1 an - 25% de données",
                      " échantillons")) |> 
  add_row(en = paste0("Total - Mobility status 5 years ago - 25% sample data"), 
          fr = paste0("Total - Statut de mobilité il y a 5 ans - 25% des donné",
                      "es de l'échantillon")) |> 
  add_row(en = paste0("Occupied private dwellings by structural type of dwelli",
                      "ng data"), 
          fr = paste0("Données sur les logements privés occupés par type struc",
                      "turel de logement")) |> 
  add_row(en = paste0("Total - Income statistics in 2015 for private household",
                      "s by household size - 100% data"), 
          fr = paste0("Total - Statistiques sur les revenus en 2015 des ménage",
                      "s privés selon la taille du ménage - données à 100%.")) |> 
  add_row(en = paste0("Total - Household total income groups in 2015 for priva",
                      "te households - 100% data"), 
          fr = paste0("Total - Groupes de revenus totaux des ménages en 2015 p",
                      "our les ménages privés - données à 100%.")) |> 
  add_row(en = paste0("Total - Low-income status in 2015 for the population in",
                      " private households to whom low-income concepts are app",
                      "licable - 100% data"), 
          fr = paste0("Total - Statut de bas revenu en 2015 pour la population",
                      " des ménages privés à laquelle s'appliquent les concept",
                      "s de bas revenu - données à 100%.")) |> 
  add_row(en = paste0("Total - Immigrant status and period of immigration for ",
                      "the population in private households - 25% sample data"), 
          fr = paste0("Total - Statut d'immigrant et période d'immigration pou",
                      "r la population des ménages privés - Données-échantillo",
                      "n (25%)")) |> 
  add_row(en = paste0("Total - Visible minority for the population in private ",
                      "households - 25% sample data"), 
          fr = paste0("Total - Minorité visible pour la population dans les mé",
                      "nages privés - données de l'échantillon de 25%.")) |> 
  add_row(en = paste0("Total - Aboriginal identity for the population in priva",
                      "te households - 25% sample data"), 
          fr = paste0("Total - Identité autochtone pour la population dans les",
                      " ménages privés - Données échantillon (25%)")) |> 
  add_row(en = paste0("Total - Main mode of commuting for the employed labour ",
                      "force aged 15 years and over in private households with",
                      " a usual place of work or no fixed workplace address - ",
                      "25% sample data"), 
          fr = paste0("Total - Mode de transport principal pour les actifs occ",
                      "upés âgés de 15 ans et plus dans les ménages privés aya",
                      "nt un lieu de travail habituel ou sans adresse de trava",
                      "il fixe - Données-échantillon (25%)")) |> 
  add_row(en = paste0("Total - Commuting duration for the employed labour forc",
                      "e aged 15 years and over in private households with a u",
                      "sual place of work or no fixed workplace address - 25% ",
                      "sample data"), 
          fr = paste0("Total - Durée du trajet domicile-travail pour les actif",
                      "s occupés âgés de 15 ans et plus dans les ménages privé",
                      "s ayant un lieu de travail habituel ou sans adresse de ",
                      "travail fixe - Données-échantillon (25%)")) |> 
  add_row(en = paste0("All industry categories"), 
          fr = paste0("Toutes les catégories d'industries")) |> 
  add_row(en = paste0("Total - Private households by household type - 100% dat",
                      "a"), 
          fr = paste0("Total - Ménages privés par type de ménage - Données à 1",
                      "00%.")) |> 
  add_row(en = paste0("Total - Knowledge of official languages for the total p",
                      "opulation excluding institutional residents - 100% data",
                      ""), 
          fr = paste0("Total - Connaissance des langues officielles pour la po",
                      "pulation totale, à l'exclusion des résidents institutio",
                      "nnels - Données à 100%.")) |> 
  add_row(en = paste0("Total - Age"), 
          fr = paste0("Total - Âge")) |> 
  add_row(en = paste0("Total - Highest certificate, diploma or degree for the ",
                      "population aged 15 years and over in private households",
                      " - 25% sample data"), 
          fr = paste0("Total - Plus haut certificat, diplôme ou grade pour la ",
                      "population âgée de 15 ans et plus dans les ménages priv",
                      "és - Données-échantillon (25%)"))


