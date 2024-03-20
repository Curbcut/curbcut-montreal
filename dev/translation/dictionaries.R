# scales_dictionary <- qs::qread("data/scales_dictionary.qs")
# 
# z <- lapply(scales_dictionary$place_name, \(x) {
#   fr <- .t(x)
#   
#   sprintf('add_row(en = "%s",
#           fr = "%s")', x, fr)
# })
# 
# paste0(z, collapse = " |>\n") |>
#   writeLines()
# 

# Scales dictionary -------------------------------------------------------

translation_dictionaries <- tibble(en = character(),
                                   fr = character()) |> 
  # sing
  add_row(en = "borough/city",
          fr = "arrondissement/ville") |>
  add_row(en = "census tract",
          fr = "secteur de recensement") |>
  add_row(en = "dissemination area",
          fr = "aire de diffusion") |>
  add_row(en = "building",
          fr = "bâtiment") |>
  add_row(en = "CMHC zone",
          fr = "zone SCHL") |>
  add_row(en = "Centraide zone",
          fr = "zone Centraide") |>
  add_row(en = "area at the 25m scale",
          fr = "zone à l'échelle de 25m") |>
  add_row(en = "area at the 50m scale",
          fr = "zone à l'échelle de 50m") |>
  add_row(en = "area at the 100m scale",
          fr = "zone à l'échelle de 100m") |>
  add_row(en = "area at the 250m scale",
          fr = "zone à l'échelle de 250m") |> 
  add_row(en = "area at the 30m scale",
          fr = "zone à l'échelle de 30m") |>
  add_row(en = "area at the 60m scale",
          fr = "zone à l'échelle de 60m") |>
  add_row(en = "area at the 120m scale",
          fr = "zone à l'échelle de 120m") |>
  add_row(en = "area at the 300m scale",
          fr = "zone à l'échelle de 300m") |> 
  add_row(en = "area at the 600m scale", 
          fr = "zone à l'échelle de 600m") |> 
add_row(en = "city",
        fr = "ville") |>
  add_row(en = "borough",
          fr = "arrondissement") |>
  add_row(en = "integrated (university) health and social services centre",
          fr = "centre intégré (universitaire) de santé et de services sociaux") |>
  add_row(en = "local community service centre",
          fr = "centre local de service communautaire") |>
  add_row(en = "dissemination block", 
          fr = "îlot de diffusion") |> 
  add_row(en = "territorial service network", 
          fr = "réseau territorial de services") |> 
  add_row(en = "Table de quartier", 
          fr = "Table de quartier") |> 
  add_row(en = "rasters (30m*30m)", 
          fr = "mailles (30m*30m)") |> 
  
  # sing_with_article
  add_row(en = "the borough/city",
          fr = "l'arrondissement/ville") |>
  add_row(en = "the census tract",
          fr = "le secteur de recensement") |>
  add_row(en = "the dissemination area",
          fr = "l'aire de diffusion") |>
  add_row(en = "the building",
          fr = "le bâtiment") |>
  add_row(en = "the CMHC zone",
          fr = "la Zone SCHL") |>
  add_row(en = "the Centraide zone",
          fr = "la Zone centraide") |>
  add_row(en = "the area at the 25m scale",
          fr = "la zone à l'échelle de 25m") |>
  add_row(en = "the area at the 50m scale",
          fr = "la zone à l'échelle de 50m") |>
  add_row(en = "the area at the 100m scale",
          fr = "la zone à l'échelle de 100m") |>
  add_row(en = "the area at the 250m scale",
          fr = "la zone à l'échelle de 250m") |> 
  add_row(en = "the area at the 30m scale",
          fr = "la zone à l'échelle de 30m") |>
  add_row(en = "the area at the 60m scale",
          fr = "la zone à l'échelle de 60m") |>
  add_row(en = "the area at the 120m scale",
          fr = "la zone à l'échelle de 120m") |>
  add_row(en = "the area at the 300m scale",
          fr = "la zone à l'échelle de 300m") |> 
  add_row(en = "the area at the 600m scale",
          fr = "la zone à l'échelle de 600m") |> 
  add_row(en = "the city",
          fr = "la ville") |>
  add_row(en = "the borough",
          fr = "l'arrondissement") |>
  add_row(en = "the integrated (university) health and social services centre",
          fr = "le centre intégré (universitaire) de santé et de services sociaux") |>
  add_row(en = "the local community service centre",
          fr = "le centre local de service communautaire") |>
  
  add_row(en = "the dissemination block", 
          fr = "l'îlot de diffusion") |> 
  add_row(en = "the territorial service network", 
          fr = "le réseau territorial de services") |> 
  add_row(en = "the Table de quartier", 
          fr = "the Table de quartier") |> 
  
  # plur
  add_row(en = "boroughs or cities",
          fr = "arrondissements ou villes") |>
  add_row(en = "census tracts",
          fr = "secteurs de recensement") |>
  add_row(en = "dissemination areas",
          fr = "aires de diffusion") |>
  add_row(en = "buildings",
          fr = "bâtiments") |>
  add_row(en = "CMHC zones",
          fr = "zones SCHL") |>
  add_row(en = "Centraide zones",
          fr = "zones centraide") |>
  add_row(en = "areas at the 25m scale",
          fr = "zones à l'échelle de 25m") |>
  add_row(en = "areas at the 50m scale",
          fr = "zones à l'échelle de 50m") |>
  add_row(en = "areas at the 100m scale",
          fr = "zones à l'échelle de 100m") |>
  add_row(en = "areas at the 250m scale",
          fr = "zones à l'échelle de 250m") |> 
  add_row(en = "areas at the 30m scale",
          fr = "zones à l'échelle de 30m") |>
  add_row(en = "areas at the 60m scale",
          fr = "zones à l'échelle de 60m") |>
  add_row(en = "areas at the 120m scale",
          fr = "zones à l'échelle de 120m") |>
  add_row(en = "areas at the 300m scale",
          fr = "zones à l'échelle de 300m") |> 
  add_row(en = "areas at the 600m scale",
          fr = "zones à l'échelle de 600m") |> 
  add_row(en = "cities",
          fr = "villes") |>
  add_row(en = "boroughs",
          fr = "arrondissements") |>
  add_row(en = "integrated (university) health and social services centres",
          fr = "centres intégrés (universitaires) de santé et de services sociaux") |>
  add_row(en = "local community services centres",
          fr = "centres locaux de services communautaires") |>
  add_row(en = "dissemination blocks", 
          fr = "îlots de diffusion") |> 
  add_row(en = "territorial service networks", 
          fr = "réseaux territoriaux de services") |> 
  add_row(en = "Table de quartier", 
          fr = "Table de quartier") |> 
  
  # slider title
  add_row(en = "Borough/City",
          fr = "Arrondissement/Ville") |>
  add_row(en = "Census tract",
          fr = "Secteur de recensement") |>
  add_row(en = "Dissemination area",
          fr = "Aire de diffusion") |>
  add_row(en = "Building",
          fr = "Bâtiment") |>
  add_row(en = "CMHC zone",
          fr = "Zone SCHL") |>
  add_row(en = "Centraide zone",
          fr = "Zone centraide") |>
  add_row(en = "25m",
          fr = "25m") |>
  add_row(en = "50m",
          fr = "50m") |>
  add_row(en = "100m",
          fr = "100m") |>
  add_row(en = "250m",
          fr = "250m") |> 
  add_row(en = "30m",
          fr = "30m") |>
  add_row(en = "60m",
          fr = "60m") |>
  add_row(en = "120m",
          fr = "120m") |>
  add_row(en = "300m",
          fr = "300m") |> 
  add_row(en = "600m",
          fr = "600m") |> 
  add_row(en = "City",
          fr = "Ville") |>
  add_row(en = "Borough",
          fr = "Arrondissement") |>
  add_row(en = "RTS",
          fr = "RTS") |>
  add_row(en = "CLSC",
          fr = "CLSC") |>
  add_row(en = "Centraide Zone",
          fr = "Zone Centraide") |> 
  add_row(en = "Dissemination block", 
          fr = "Îlot de diffusion") |> 
  add_row(en = "Tables de quartier", 
          fr = "Tables de quartier") |> 
  add_row(en = "Table de quartier", 
          fr = "Table de quartier") |> 
  add_row(en = "Territorial service networks", 
          fr = "Réseaux territoriaux de services") |> 
  
  # place heading
  add_row(en = "{name_2} of {name}",
          fr = "{name_2} {name}") |>
  add_row(en = "Census tract {name} ({name_2})",
          fr = "Secteur de recensement {name} ({name_2})") |>
  add_row(en = "Dissemination area {name} ({name_2})",
          fr = "Aire de diffusion {name} ({name_2})") |>
  add_row(en = "{name}",
          fr = "{name}") |>
  add_row(en = "Centraide zone of {name}",
          fr = "Zone centraide {name}") |>
  add_row(en = "CLSC {name}",
          fr = "CLSC {name}") |>
  add_row(en = "Borough {name}",
          fr = "Arrondissement {name}") |>
  add_row(en = "Dissemination block {name} ({name_2})", 
          fr = "Îlot de diffusion {name} ({name_2})") |> 

# place name
add_row(en = "Census tract {name}",
        fr = "Secteur de recensement {name}") |>
  add_row(en = "Dissemination area {name}",
          fr = "Aire de diffusion {name}") |>
  add_row(en = "the 25m grid area around {name}",
          fr = "la zone de 25 m autour du {name}") |>
  add_row(en = "the 50m grid area around {name}",
          fr = "la zone de 50 m autour du {name}") |>
  add_row(en = "the 100m grid area around {name}",
          fr = "la zone de 100 m autour du {name}") |>
  add_row(en = "the 250m grid area around {name}",
          fr = "la zone de 250 m autour du {name}") |> 
  add_row(en = "the 30m grid area around {name}",
          fr = "la zone de 30 m autour du {name}") |>
  add_row(en = "the 60m grid area around {name}",
          fr = "la zone de 60 m autour du {name}") |>
  add_row(en = "the 120m grid area around {name}",
          fr = "la zone de 120 m autour du {name}") |>
  add_row(en = "the 300m grid area around {name}",
          fr = "la zone de 300 m autour du {name}") |> 
  add_row(en = "the 600m grid area around {name}",
          fr = "la zone de 600 m autour du {name}") |> 
  add_row(en = "Dissemination block {name}", 
          fr = "Îlot de diffusion {name}") |> 
  
  # subtext
  add_row(en = paste0(
    "Census units"), 
    fr = paste0(
      "Unités de recensement")) |> 
  add_row(en = paste0(
    "Census units (Census subdivisions)"), 
    fr = paste0(
      "Unités de recensement (Subdivisions de recensement)")) |> 
  add_row(en = paste0(
    "Municipal administrations (19) in the City of Montreal"), 
    fr = paste0(
      "Administrations municipales (19) dans la ville de Montréal")) |> 
  add_row(en = paste0(
    "Municipal administrations in the City of Montreal with census subdivis", 
    "ions in the region"), 
    fr = paste0(
      "Administrations municipales de la ville de Montréal et subdivisions de", 
      " recensement de la région")) |> 
  add_row(en = paste0(
    "Territories for which the local integrated center (CISSS/CIUSSS) is re", 
    "sponsible for ensuring the development and smooth operation of the hea", 
    "lth and social services network"), 
    fr = paste0(
      "Territoires pour lesquels le centre intégré local (CISSS/CIUSSS) a la ", 
      "responsabilité d'assurer le développement et le bon fonctionnement du ", 
      "réseau de la santé et des services sociaux.")) |> 
  add_row(en = paste0(
    "Territories for which the local community service centre (CLSC) has th",
    "e mission to provide routine, front-line health and social services to",
    " the population"
  ), 
    fr = paste0(
      "Territoires pour lesquels le centre local de services communautair", 
      "es (CLSC) a pour mission d'offrir des services de santé et des service", 
      "s sociaux courants et de première ligne à la population.")) |> 
  add_row(en = paste0(
    "Designated areas in Canada for housing market analysis by Canada Mortg", 
    "age and Housing Corporation (CMHC)"), 
    fr = paste0(
      "Zones désignées au Canada pour l'analyse du marché du logement par la ", 
      "Société canadienne d'hypothèques et de logement (SCHL)")) |> 
  add_row(en = paste0(
    "Areas for Centraide's community support and development activities"), 
    fr = paste0(
      "Zones d'intervention de Centraide en matière de soutien et de dével", 
      "oppement communautaire")) |> 
  add_row(en = "Territories where local stakeholders collaborate to enhance neighborhood living quality and conditions.", 
          fr = "Territoires où les acteurs locaux collaborent pour améliorer la qualité et les conditions de vie dans le quartier") |> 
  add_row(en = paste0(
    "Small square areas, each measuring 30 meters by 30 meters"), 
    fr = paste0(
      "Petites zones carrées, mesurant chacune 30 mètres par 30 mètres")) |> 
  add_row(en = paste0(
    "Small square areas, each measuring 60 meters by 60 meters"), 
    fr = paste0(
      "Petites zones carrées, chacune mesurant 60 mètres par 60 mètres")) |> 
  add_row(en = paste0(
    "Small square areas, each measuring 120 meters by 120 meters"), 
    fr = paste0(
      "Petites zones carrées, chacune mesurant 120 mètres par 120 mètres")) |> 
  add_row(en = paste0(
    "Small square areas, each measuring 300 meters by 300 meters"), 
    fr = paste0(
      "Petites zones carrées, chacune mesurant 300 mètres par 300 mètres")) |> 
  add_row(en = paste0(
    "Small square areas, each measuring 25 meters by 25 meters"), 
    fr = paste0(
      "Petites zones carrées, mesurant chacune 25 mètres par 25 mètres")) |> 
  add_row(en = paste0(
    "Small square areas, each measuring 50 meters by 50 meters"), 
    fr = paste0(
      "Petites zones carrées, mesurant chacune 50 mètres par 50 mètres")) |> 
  add_row(en = paste0(
    "Small square areas, each measuring 100 meters by 100 meters"), 
    fr = paste0(
      "Petites zones carrées, chacune mesurant 100 mètres par 100 mètres")) |> 
  add_row(en = paste0(
    "Small square areas, each measuring 250 meters by 250 meters"), 
    fr = paste0(
      "Petites zones carrées, mesurant chacune 250 mètres sur 250 mètres")) |> 
  add_row(en = paste0(
    "Small square areas, each measuring 600 meters by 600 meters"), 
    fr = paste0(
      "Petites zones carrées, mesurant chacune 600 mètres sur 600 mètres")) |> 
  
  
  # Regions dictionary ------------------------------------------------------

# region name
add_row(en = "Montreal region",
        fr = "Région de Montréal") |>
  add_row(en = "Island of Montreal",
          fr = "Île de Montréal") |>
  add_row(en = "City of Montreal",
          fr = "Ville de Montréal") |>
  add_row(en = "Centraide of Greater Montreal",
          fr = "Centraide du Grand Montréal") |> 
  
  # to compare
  add_row(en = "in the Montreal region",
          fr = "dans la région de Montréal") |>
  add_row(en = "on the island of Montreal",
          fr = "sur l'île de Montréal") |>
  add_row(en = "in the City of Montreal",
          fr = "dans la ville de Montréal") |>
  add_row(en = "in the Centraide of Greater Montreal territory",
          fr = "sur le territoire de Centraide du Grand Montréal") |> 
  
  # to compare determ
  add_row(en = "the Montreal region",
          fr = "la région de Montréal") |>
  add_row(en = "the island of Montreal",
          fr = "l'île de Montréal") |>
  add_row(en = "the City of Montreal",
          fr = "la ville de Montréal") |>
  add_row(en = "the Centraide of Greater Montreal territory",
          fr = "le territoire de Centraide du Grand Montréal") |> 
  
  # to compare short
  add_row(en = "in the region",
          fr = "dans la région") |>
  add_row(en = "on the island",
          fr = "sur l'île") |>
  add_row(en = "in the City",
          fr = "dans la ville") |>
  add_row(en = "in the territory",
          fr = "dans le territoire")


