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
    title = "The Miron Quarry: The Transformation of an Industrial Limestone Quarry into a Flourishing Environmental Complex",
    img = "mirron_quarry.png",
    preview = "Mirron Quarry",
    lon = -73.61389,
    lat = 45.57222) |>
  
  add_row(
    name = "parc_ouest",
    title = "The Grand parc de l'ouest",
    img = "parc_ouest.png",
    preview = "The Grand parc de l'ouest",
    lon = -73.93723394534209,
    lat = 45.45384833740566) |> 
  
  add_row(
    name = "turcot_interchange",
    title = paste0("The Reconstruction of the Turcot Interchange: The ",
                   "Intersection of Community Desires and Government Priorities"),
    img = "turcot_interchange.png",
    preview = paste0("Turcot interchange"),
    lon = -73.600007,
    lat = 45.468355) |>
  
  add_row(
    name = "griffintown",
    title = paste0("Griffintown, From a Gift of the Machine Age to a Water ",
                   "Wheel in the Growth Machine"),
    img = "griffintown.png",
    preview = paste0("Griffintown, From a Gift of the Machine Age to a Water ",
                     "Wheel in the Growth Machine"),
    lon = -73.560739,
    lat = 45.496812)


# Finish table ------------------------------------------------------------
  
stories <- 
  stories |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(buffer = st_buffer(geometry, 1500)) |> 
  st_set_geometry("buffer") |> 
  mutate(ID = seq_len(n()), .before = name)
