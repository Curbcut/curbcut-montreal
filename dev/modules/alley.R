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
  rename(name = Name)

alleys_visited_processed <- 
  alleys_visited %>%
  mutate(date = as.Date(NA)) %>% 
  select(ID = name, date) %>% 
  st_transform(32618) %>% 
  st_buffer(., 2) %>%
  st_transform(4326)

# Combine files ----------------------------------------

alleys <- 
  bind_rows(alleys_mtl, alleys_google, alleys_mn, alleys_visited_processed) %>%
  mutate(ID = seq_along(ID))

alleys <- 
  alleys %>%
  rename(alley_ID = ID) %>%
  st_join(select(borough, ID)) %>%
  relocate(ID, .after = alley_ID)


# Add info about alleys visited -------------------------------------------

alleys_visited <- 
  alleys_visited %>%
  mutate(name = str_remove(name, "^\\d*\\."),
         name = str_trim(name))

alleys_visited_text <- 
  read_delim("dev/data/green_alleys/Alleys visited.csv", ";") %>%
  mutate(Photo_id = str_remove(Photo_id, "\\\n.*$")) |> 
  set_names(c("ID", "name", "type", "date", "description", "photo_ID"))

alleys_visited_text |> 
  anti_join(alleys_visited)

alleys_visited$name
alleys_visited_text$name
alleys_visited_text$ID




# Get borough text --------------------------------------------------------

alley_text <- 
  read_csv("dev/data/green_alleys/Info by borough.csv") %>%
  drop_na() %>%
  set_names(c("name", "description")) %>%
  mutate(name = case_when(
    name == "Côte des Neiges - Notre Dame de Grâce" ~ 
      "Côte-des-Neiges-Notre-Dame-de-Grâce",
    name == "Plateau-Mont-Royal" ~ "Le Plateau-Mont-Royal",
    name == "Sud-Ouest" ~ "Le Sud-Ouest",
    name == "Rosemont-La Petite Patrie" ~ "Rosemont-La Petite-Patrie",
    TRUE ~ name
  )) %>%
  left_join(st_drop_geometry(select(borough, name, ID)))





# Clean up ----------------------------------------------------------------

rm(alleys_mtl, alleys_google, alleys_mn, alleys_visited)
