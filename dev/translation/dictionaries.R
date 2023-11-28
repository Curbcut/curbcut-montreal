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
  add_row(en = "city",
          fr = "ville") |>
  add_row(en = "borough",
          fr = "arrondissement") |>
  add_row(en = "integrated (university) health and social services centre",
          fr = "centre intégré (universitaire) de santé et de services sociaux") |>
  add_row(en = "local community service centre",
          fr = "centre local de service communautaire") |>
  
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
  add_row(en = "the city",
          fr = "la ville") |>
  add_row(en = "the borough",
          fr = "l'arrondissement") |>
  add_row(en = "the integrated (university) health and social services centre",
          fr = "le centre intégré (universitaire) de santé et de services sociaux") |>
  add_row(en = "the local community service centre",
          fr = "le centre local de service communautaire") |>
  
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
  add_row(en = "cities",
          fr = "villes") |>
  add_row(en = "boroughs",
          fr = "arrondissements") |>
  add_row(en = "integrated (university) health and social services centres",
          fr = "centres intégrés (universitaires) de santé et de services sociaux") |>
  add_row(en = "local community services centres",
          fr = "centres locaux de services communautaires") |>
  
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
  add_row(en = "City",
          fr = "Ville") |>
  add_row(en = "Borough",
          fr = "Arrondissement") |>
  add_row(en = "CIUSSS",
          fr = "CIUSSS") |>
  add_row(en = "CLSC",
          fr = "CLSC") |>
  add_row(en = "Centraide Zone",
          fr = "Zone Centraide") |> 
  
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
  
  
  # Regions dictionary ------------------------------------------------------

# region name
add_row(en = "Metropolitan Area",
        fr = "Région métropolitaine") |>
  add_row(en = "Island of Montreal",
          fr = "Île de Montréal") |>
  add_row(en = "City of Montreal",
          fr = "Ville de Montréal") |>
  add_row(en = "Centraide of Greater Montreal",
          fr = "Centraide du Grand Montréal") |>
  add_row(en = "Canada Mortgage and Housing Corporation zones",
          fr = "Zones de la Société canadienne d'hypothèques et de logement") |>
  add_row(en = "250-m",
          fr = "250-m") |> 
  
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


