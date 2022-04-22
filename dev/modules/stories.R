#### Stories data setup ########################################################

# This script is totally independent

# Create key tibble -------------------------------------------------------

stories <- tibble(
  name = character(),
  title = character(),
  img = character(),
  preview = character(),
  lon = numeric(),
  lat = numeric()) 


# Adding stories ----------------------------------------------------------

stories <- 
  stories |> 
  
  add_row(
    name = "little_burgundy",
    title = paste0("Environmental racism and green gentrification in ",
                   "Montreal’s Little Burgundy"),
    img = "little_burgundy.png",
    preview = paste0("Little Burgundy has been the site of nearly 100 years ",
                     "of social and environmental transformation, and is ",
                     "currently undergoing a wave of green gentrification."),
    lon = -73.574962, 
    lat = 45.479311) |> 
  
  add_row(
    name = "mirron_quarry",
    title = paste0("The Miron Quarry: The transformation of an industrial ",
                   "limestone quarry into a flourishing environmental complex"),
    img = "mirron_quarry.png",
    preview = paste0("Residents and government are coming together to shape ",
                     "the future of the Saint-Michel Environmental Complex, ",
                     "located on the site of the former Miron Quarry."),
    lon = -73.61389,
    lat = 45.57222) |>
  
  add_row(
    name = "parc_ouest",
    title = "Understanding what lies beneath Montreal’s Grand Parc de L’Ouest",
    img = "parc_ouest.png",
    preview = paste0("In the Grand Parc de L’Ouest, a renewed drive to ",
                     "protect biodiversity and create greenspace illustrates ",
                     "the changing politics of development on the island."),
    lon = -73.93723394534209,
    lat = 45.45384833740566) |> 
  
  add_row(
    name = "turcot_interchange",
    title = paste0("The reconstruction of the Turcot Interchange: The ",
                   "intersection of community desires and government ",
                   "priorities"),
    img = "turcot_interchange.png",
    preview = paste0("The City of Montreal claims it is looking to support ",
                     "pedestrian-oriented projects as a pathway towards ",
                     "sustainable mobility, but contradictions in the ",
                     "implementation stage of the Turcot Interchange suggest ",
                     "otherwise."),
    lon = -73.600007,
    lat = 45.468355) |>
  
  add_row(
    name = "griffintown",
    title = paste0("Griffintown, From a gift of the machine age to a water ",
                   "wheel in the growth machine"),
    img = "griffintown.png",
    preview = paste0("Griffintown’s social and governance history can be ",
                     "defined through the politics of water management."),
    lon = -73.560739,
    lat = 45.496812)


# Finish table ------------------------------------------------------------
  
stories <- 
  stories |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(buffer = st_buffer(geometry, 1500)) |> 
  st_set_geometry("buffer") |> 
  mutate(ID = seq_len(n()), .before = name)
