#### Stories data setup #########################################################

# This script is totally independent

# Create key tibble -------------------------------------------------------
stories <- 
tibble(name = "Environmental Racism and Green Gentrification in Montreal’s Little Burgundy",
       rmd = "little_burgundy", # Forget the .Rmd since there will be _en.rmd and _fr.rmd
       img = "little_burgundy.png",
       lon = -73.57450068915936, 
       lat = 45.486876630300735) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(buffer = st_buffer(geometry, 500)) %>% 
  st_set_geometry("buffer") %>% 
  mutate(ID = seq(1:n()), .before = name)
