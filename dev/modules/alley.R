#### Green alley data setup #################################################### 

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

green_space <- 
  read_sf("dev/data/green_alleys/Espace_Vert.shp") %>% 
  select(ID = OBJECTID, name = Nom, type_1 = TYPO1, type_2 = TYPO2, geometry) %>% 
  st_transform(4326) %>% 
  st_set_agr("constant") %>% 
  st_cast("POLYGON")

# File from Donnees Montreal
alleys_mtl <-
  read_sf("dev/data/green_alleys/ruelles-vertes.shp") %>%
  st_transform(32618) %>%
  count(RUELLE_ID, DATE_AMENA) %>%
  rename(ID = RUELLE_ID, date = DATE_AMENA) %>%
  group_by(ID) %>%
  summarize(date = date[n == max(n) & date == min(date[n == max(n)])]) %>%
  st_transform(4326) %>%
  mutate(date = as.Date(date, "%Y%m%d"))

# File from Google Earth
alleys_google <-
  read_sf("dev/data/green_alleys/RV_OnlyGM.shp") %>%
  mutate(date = as.Date(NA)) %>% 
  select(ID = Name, date, geometry) %>%
  st_transform(32618) %>% 
  st_buffer(., 2) %>%
  st_transform(4326)

# File from Montreal Nord
alleys_mn <-
  read_sf("dev/data/green_alleys/Montréal-NordRV.shp") %>%
  mutate(date = as.Date(NA)) %>% 
  select(ID = Name, date) %>% 
  st_transform(32618) %>% 
  st_buffer(., 2) %>%
  st_transform(4326)

# Alleys visited
alleys_visited <- 
  read_sf("dev/data/green_alleys/RV visited.shp") %>%
  mutate(date = as.Date(NA)) %>% 
  select(ID = Name, date) %>% 
  st_transform(32618) %>% 
  st_buffer(., 2) %>%
  st_transform(4326)

# Combine files ----------------------------------------

alleys <- 
  bind_rows(alleys_mtl, alleys_google, alleys_mn, alleys_visited) %>%
  mutate(ID = seq_along(ID))

alleys <- 
  alleys %>%
  rename(alley_ID = ID) %>%
  st_join(select(borough, ID)) %>%
  relocate(ID, .after = alley_ID)


# Get borough text --------------------------------------------------------

alley_text <- 
  read_delim("dev/data/green_alleys/Info by borough.csv", ";")


# Clean up ----------------------------------------------------------------

rm(alleys_mtl, alleys_google, alleys_mn, alleys_visited)

alleys |> 
  rename(paste0("alley", "test") = ID)
