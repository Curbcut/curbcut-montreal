## BUILD STORIES ###############################################################

build_stories <- function() {
  
  # Build empty table -------------------------------------------------------
  
  stories <- stories_empty_table()
  
  # Add every story ---------------------------------------------------------
  
  stories <- 
    stories |> 
    stories_add_story(
      name_id = "little_burgundy",
      title = paste0("Environmental racism and green gentrification in ",
                     "Montreal’s Little Burgundy"),
      short_title = "Little Burgundy's transformation",
      preview_en = paste0("Little Burgundy has been the site of nearly 100 years ",
                       "of social and environmental transformation, and is ",
                       "currently undergoing a wave of green gentrification."),
      preview_fr = paste0("Depuis près de 100 ans, la Petite Bourgogne est le ",
                          "lieu d'une transformation sociale et environnementale ",
                          "et connaît actuellement une vague d'éco-gentrification."),
      themes = c("Gentrification", "Community activism", "Urban renewal"),
      lon = -73.574962, 
      lat = 45.479311) |> 
    
    stories_add_story(
      name_id = "mirron_quarry",
      title = paste0("The Miron Quarry: The transformation of an industrial ",
                     "limestone quarry into a flourishing environmental complex"),
      short_title = "The Miron Quarry",
      preview_en = paste0("Residents and government are coming together to shape ",
                       "the future of the Saint-Michel Environmental Complex, ",
                       "located on the site of the former Miron Quarry."),
      preview_fr = paste0("Les résidents et le gouvernement se réunissent pour ",
                          "façonner l'avenir du Complexe environnemental de ",
                          "Saint-Michel, situé sur le site de l'ancienne carrière ",
                          "Miron."),
      themes = c("Green space", "Urban transformation", "Community activism"),
      lon = -73.62876,
      lat = 45.56256) |>
    
    stories_add_story(
      name_id = "parc_ouest",
      title = "Understanding what lies beneath Montreal’s Grand Parc de L’Ouest",
      short_title = "Grand Parc de l'Ouest",
      preview_en = paste0("In the Grand Parc de L’Ouest, a renewed drive to ",
                       "protect biodiversity and create greenspace illustrates ",
                       "the changing politics of development on the island."),
      preview_fr = paste0("Dans le Grand Parc de l'Ouest, la volonté renouvelée ",
                          "de protéger la biodiversité et de créer des espaces ",
                          "verts illustre l'évolution des politiques de ",
                          "développement sur l'île."),
      themes = c("Biodiversity", "Green space", "Urban governance"),
      lon = -73.93723394534209,
      lat = 45.45384833740566) |> 
    
    stories_add_story(
      name_id = "turcot_interchange",
      title = paste0("The reconstruction of the Turcot Interchange: The ",
                     "intersection of community desires and government ",
                     "priorities"),
      short_title = "The Turcot interchange",
      preview_en = paste0("The City of Montreal claims it is looking to support ",
                       "pedestrian-oriented projects as a pathway towards ",
                       "sustainable mobility, but contradictions in the ",
                       "implementation stage of the Turcot Interchange suggest ",
                       "otherwise."),
      preview_fr = paste0("La ville de Montréal affirme qu'elle cherche à ",
                          "soutenir les projets axés sur les piétons comme ",
                          "une voie vers la mobilité durable, mais des contradictions ",
                          "dans la phase de mise en œuvre de l'échangeur Turcot ",
                          "suggèrent le contraire."),
      themes = c("Transportation", "Community activism", 
                 "Urban governance"),
      lon = -73.600007,
      lat = 45.468355) |>
    
    stories_add_story(
      name_id = "griffintown",
      title = paste0("Griffintown, From a gift of the machine age to a water ",
                     "wheel in the growth machine"),
      short_title = "Griffintown's water management history",
      preview_en = paste0("Griffintown’s social and governance history can be ",
                       "defined through the politics of water management."),
      preview_fr = paste0("L'histoire sociale et de gouvernance de Griffintown ",
                          "peut être définie par la politique de gestion de l'eau."),
      themes = c("Urban governance", "Urban transformation"),
      lon = -73.560739,
      lat = 45.496812) |> 
    
    stories_add_story(
      name_id = "champs_possibles",
      title = paste0("The Champ des Possibles: A Communitarian & Biodiverse ",
                     "Urban Prairie"),
      short_title = "The Champ des Possibles",
      preview_en = paste0("An urban prairie located in the Mile End, the Champ des ",
                       "Possibles is a biodiverse green space that has a unique ",
                       "governance structure that was born out of community ",
                       "actions."),
      preview_fr = paste0("Prairie urbaine située dans le Mile End, le Champ ",
                          "des Possibles est un espace vert biodiversifié doté ",
                          "d'une structure de gouvernance unique née des actions ",
                          "de la communauté."),
      themes = c("Green space", "Urban transformation", "Community activism", 
                 "Biodiversity"),
      lon = -73.600377,
      lat = 45.528423) |> 
    
    stories_add_story(
      name_id = "metro_evolution",
      title = paste0("The Evolution of the Montreal Metro"),
      short_title = "Evolution of the metro",
      preview_en = paste0("Since its inception in the early 20th century, the ",
                          "Montreal metro has undergone several major ",
                          "transformations that mirror the city’s ever-changing ",
                          "sociopolitical landscape."),
      preview_fr = paste0("Depuis sa création au début du XXe siècle, le métro ",
                          "de Montréal a subi plusieurs transformations ",
                          "majeures qui reflètent l'évolution constante du ",
                          "paysage sociopolitique de la ville."),
      themes = c("Transportation", "Urban governance"),
      lon = -73.628745,
      lat = 45.542925) |> 
    
    stories_add_story(
      name_id = "green_alleys_plateau",
      title = paste0("The Green Alley Program in Le Plateau-Mont-Royal: tensions ",
                     "between beautification and privatisation of public space"),
      short_title = "Green alley program in Le Plateau-Mont-Royal",
      preview_en = paste0("Once redeveloped, green alleys often tend to become ",
                          "ambiguous spaces, a sort of common, between the public ",
                          "and the private realm. The case of Le Plateau-Mont-Royal ",
                          "is particularly interesting."),
      preview_fr = paste0("Une fois réaménagées, les ruelles vertes tendent ",
                          "souvent à devenir des espaces ambigus, des sortes ",
                          "de communs entre le domaine public et le domaine ",
                          "privé. Le cas du Plateau-Mont-Royal est ",
                          "particulièrement intéressant."),
      themes = c("Green space", "Urban transformation", "Community activism"),
      lon = -73.570753,
      lat = 45.515949) |> 
    
    stories_add_story(
      name_id = "cycling_infrastructure",
      title = paste0("A History of Cycling Infrastructure in Montréal"),
      short_title = "Cycling infrastructure",
      preview_en = paste0("Montréal is a world-renowned cycling city, but it hasn’t ",
                          "always been this way. Explore how the city’s cycling ",
                          "infrastructure has evolved over time in this interactive ",
                          "story."),
      preview_fr = paste0("Montréal est une ville cyclable de renommée mondiale, ",
                          "mais il n'en a pas toujours été ainsi. Découvrez ",
                          "comment les infrastructures cyclables de la ville ont ",
                          "évolué au fil du temps dans ce récit interactif."),
      themes = c("Transportation", "Urban governance", "Community activism"),
      lon = -73.570810,
      lat = 45.536313) |> 
    
    stories_add_story(
      name_id = "alley_strategy",
      title = paste0("Montreal alleys as a climate emergency adaptation strategy"),
      short_title = "Green alleys as climate adaptation",
      preview_en = paste0("Transforming alleys to improve people's quality of life ",
                          "on a neighbourhood scale can simultaneously enhance ",
                          "urban quality on a larger scale"),
      preview_fr = paste0("Transformer les ruelles pour améliorer la qualité ",
                          "de vie des habitants à l'échelle d'un quartier peut ",
                          "simultanément améliorer la qualité urbaine à plus ",
                          "grande échelle."),
      themes = c("Green space", "Urban transformation", "Community activism", 
                 "Biodiversity", "Climate change"),
      lon = -73.553092,
      lat = 45.480989) |> 
    
    stories_add_story(
      name_id = "mcp",
      title = paste0("Montreal Climate Plans"),
      short_title = "Montreal climate plans",
      preview_en = paste0("Climate action in Montreal: exploring two ",
                          "administrations' strategies for a greener future"),
      preview_fr = paste0("L'action climatique à Montréal : exploration des ",
                          "stratégies de deux administrations pour un avenir plus vert"),
      themes = c("Transportation", "Community activism", 
                 "Urban governance", "Climate change",
                 "Green space", "Policy"),
      lon = -73.617951,
      lat = 45.499533) |> 
    
    stories_add_story(
      name_id = "mount_royal",
      title = paste0("Mount-Royal: One Consistent Beautiful Mountain"),
      short_title = "The Mount-Royal",
      preview_en = paste0("Throughout its evolution, Mount-Royal has played an ",
                          "important role in the city of Montreal’s history and ",
                          "development"),
      preview_fr = paste0("Tout au long de son évolution, le Mont-Royal a ",
                          "joué un rôle important dans l'histoire et le ",
                          "développement de la ville de Montréal"),
      themes = c("Green space", "Urban transformation", 
                 "Biodiversity", "Climate change"),
      lon = -73.593526,
      lat = 45.502604) |>
  
  stories_add_story(
    name_id = "rem",
    title = paste0("The REM and the public-private-partnership behind it"),
    short_title = "The REM's governance model",
    preview_en = paste0("One of the controversies surrounding the REM has ", 
                        "been the lack of transparency in the governance ", 
                        "model behind the project"),
    preview_fr = paste0("L'une des controverses entourant le REM ",
                        "a été le manque de transparence du modèle ",
                        "de gouvernance qui sout-tend le projet"),
    themes = c("Transportation", "Urban governance", "Urban transformation", 
               "Policy"),
    lon = -73.430677,
    lat = 45.438482)
  
  
  # Create images and mapping -----------------------------------------------

  # IMPORTANT ON EVERY STORIES IMPORT!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  stories <- stories_map_images(stories = stories)


  # # Knit all stories Rmds ---------------------------------------------------
  # 
  # library(here)
  # if (is.null(packageDescription("leaflet")$RemoteUsername) ||
  #     packageDescription("leaflet")$RemoteUsername != "dmurdoch") {
  #   stop("Special version of the leaflet package must be installed to knit this story succesfully. Run devtools::install_github('dmurdoch/leaflet@crosstalk4')")
  # }
  # cc.buildr::stories_knit_all()
  
  
  # Return ------------------------------------------------------------------
  
  return(stories)
  
}
