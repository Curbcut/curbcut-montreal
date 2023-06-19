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
      short_title = "Little Burgundy's Transformation",
      preview = paste0("Little Burgundy has been the site of nearly 100 years ",
                       "of social and environmental transformation, and is ",
                       "currently undergoing a wave of green gentrification."),
      themes = c("Gentrification", "Community activism", "Urban renewal"),
      lon = -73.574962, 
      lat = 45.479311) |> 
    
    stories_add_story(
      name_id = "mirron_quarry",
      title = paste0("The Miron Quarry: The transformation of an industrial ",
                     "limestone quarry into a flourishing environmental complex"),
      short_title = "The Miron Quarry",
      preview = paste0("Residents and government are coming together to shape ",
                       "the future of the Saint-Michel Environmental Complex, ",
                       "located on the site of the former Miron Quarry."),
      themes = c("Green space", "Urban transformation", "Community activism"),
      lon = -73.62876,
      lat = 45.56256) |>
    
    stories_add_story(
      name_id = "parc_ouest",
      title = "Understanding what lies beneath Montreal’s Grand Parc de L’Ouest",
      short_title = "Grand Parc de l'Ouest",
      preview = paste0("In the Grand Parc de L’Ouest, a renewed drive to ",
                       "protect biodiversity and create greenspace illustrates ",
                       "the changing politics of development on the island."),
      themes = c("Biodiversity", "Green space", "Urban governance"),
      lon = -73.93723394534209,
      lat = 45.45384833740566) |> 
    
    stories_add_story(
      name_id = "turcot_interchange",
      title = paste0("The reconstruction of the Turcot Interchange: The ",
                     "intersection of community desires and government ",
                     "priorities"),
      short_title = "The Turcot Interchange",
      preview = paste0("The City of Montreal claims it is looking to support ",
                       "pedestrian-oriented projects as a pathway towards ",
                       "sustainable mobility, but contradictions in the ",
                       "implementation stage of the Turcot Interchange suggest ",
                       "otherwise."),
      themes = c("Transportation", "Community activism", 
                 "Urban governance"),
      lon = -73.600007,
      lat = 45.468355) |>
    
    stories_add_story(
      name_id = "griffintown",
      title = paste0("Griffintown, From a gift of the machine age to a water ",
                     "wheel in the growth machine"),
      short_title = "Griffintown's water management history",
      preview = paste0("Griffintown’s social and governance history can be ",
                       "defined through the politics of water management."),
      themes = c("Urban governance ", "Urban transformation"),
      lon = -73.560739,
      lat = 45.496812) |> 
    
    stories_add_story(
      name_id = "champs_possibles",
      title = paste0("The Champ des Possibles: A Communitarian & Biodiverse ",
                     "Urban Prairie"),
      short_title = "The Champ des Possibles",
      preview = paste0("An urban prairie located in the Mile End, the Champ des ",
                       "Possibles is a biodiverse green space that has a unique ",
                       "governance structure that was born out of community ",
                       "actions."),
      themes = c("Green space", "Urban transformation", "Community activism", 
                 "Biodiversity"),
      lon = -73.600377,
      lat = 45.528423) |> 
    
    stories_add_story(
      name_id = "metro_evolution",
      title = paste0("The Evolution of the Montreal Metro"),
      short_title = "Evolution of the Metro",
      preview = paste0("Since its inception in the early 20th century, the ",
                       "Montreal metro has undergone several major ",
                       "transformations that mirror the city’s ever-changing ",
                       "sociopolitical landscape."),
      themes = c("Transportation", "Urban governance"),
      lon = -73.628745,
      lat = 45.542925) |> 
    
    stories_add_story(
      name_id = "green_alleys_plateau",
      title = paste0("The Green Alley Program in Le Plateau-Mont-Royal: tensions ",
                     "between beautification and privatisation of public space"),
      short_title = "Green Alley Program in Plateau-Mont-Royal",
      preview = paste0("Once redeveloped, green alleys often tend to become ",
                       "ambiguous spaces, a sort of common, between the public ",
                       "and the private realm. The case of Le Plateau-Mont-Royal ",
                       "is particularly interesting."),
      themes = c("Green space", "Urban transformation", "Community activism"),
      lon = -73.570753,
      lat = 45.515949) |> 
    
    stories_add_story(
      name_id = "cycling_infrastructure",
      title = paste0("A History of Cycling Infrastructure in Montréal"),
      short_title = "Cycling infrastructure",
      preview = paste0("Montréal is a world-renowned cycling city, but it hasn’t ",
                       "always been this way. Explore how the city’s cycling ",
                       "infrastructure has evolved over time in this interactive ",
                       "story."),
      themes = c("Transportation", "Urban governance", "Community activism"),
      lon = -73.570810,
      lat = 45.536313) |> 
    
    stories_add_story(
      name_id = "alley_strategy",
      title = paste0("Montreal alleys as a climate emergency adaptation strategy"),
      short_title = "Climate Emergency Adaptation Strategy",
      preview = paste0("Transforming alleys to improve people's quality of life ",
                       "on a neighbourhood scale can simultaneously enhance ",
                       "urban quality on a larger scale"),
      themes = c("Green space", "Urban transformation", "Community activism", 
                 "Biodiversity", "Climate change"),
      lon = -73.553092,
      lat = 45.480989) |> 
    
    stories_add_story(
      name_id = "mcp",
      title = paste0("Montreal Climate Plans"),
      short_title = "Montreal Climate Plans",
      preview = paste0("Climate action in Montreal: exploring two ",
                       "administrations' strategies for a greener future"),
      themes = c("Transportation", "Community activism", 
                 "Urban governance", "Climate change",
                 "Green space", "Policy"),
      lon = -73.617951,
      lat = 45.499533) |> 
    
    stories_add_story(
      name_id = "mount_royal",
      title = paste0("Mount-Royal: One Consistent Beautiful Mountain"),
      short_title = "The Mount-Royal",
      preview = paste0("Throughout its evolution, Mount-Royal has played an ",
                       "important role in the city of Montreal’s history and ",
                       "development"),
      themes = c("Green space", "Urban transformation", 
                 "Biodiversity", "Climate change"),
      lon = -73.593526,
      lat = 45.502604)

  
  # Create images and mapping -----------------------------------------------
  
  stories_mapping <- stories_atlas_mapping(stories = stories)
  
  
  # Knit all stories Rmds ---------------------------------------------------

  library(here)
  cc.buildr::stories_knit_all()
  
  
  # Return ------------------------------------------------------------------
  
  return(list(stories = stories,
              stories_mapping = stories_mapping))
  
}
