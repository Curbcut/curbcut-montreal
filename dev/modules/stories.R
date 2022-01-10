#### Stories data setup #########################################################

# This script is totally independent

# Create key tibble -------------------------------------------------------

stories <- tibble(
  name = "Environmental Racism and Green Gentrification in Montreal’s Little Burgundy",
  rmd = "little_burgundy", # Forget the .Rmd since there will be _en.rmd and _fr.rmd
  img = "little_burgundy.png",
  lon = -73.57450068915936, 
  lat = 45.486876630300735) 


# Adding stories ----------------------------------------------------------

stories <- 
  stories %>% 
  add_row(name = "The old quarry",
          rmd = "old_quarry",
          img = "old_quarry.png",
          lon = -73.61389,
          lat = 45.57222) %>% 
  add_row(name = "The Grand parc de l'ouest",
          rmd = "parc_ouest",
          img = "parc_ouest.png",
          lon = -73.93723394534209,
          lat = 45.45384833740566) %>% 
  add_row(name = "Flooding in Pierrefonds",
          rmd = "flooding_pierrefonds",
          img = "flooding_pierrefonds.png",
          lon = -73.85489584811597,
          lat = 45.49182349238028)


# Last treatment ----------------------------------------------------------

stories <- 
  stories %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(buffer = st_buffer(geometry, 1500)) %>% 
  st_set_geometry("buffer") %>% 
  mutate(ID = seq(1:n()), .before = name)
