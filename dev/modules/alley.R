#### Green alley data setup #################################################### 

# This script relies on objects created in dev/census.R

# Get updated data from open data portal ----------------------------------

dl_unzip <- function(shp_url, name) {
  download.file(shp_url, 
                destfile = paste0("dev/data/green_alleys/",
                                "temp", ".zip"))
  
  unzip(paste0("dev/data/green_alleys/", "temp", ".zip"),
        exdir = "dev/data/green_alleys")
  
  unlink(paste0("dev/data/green_alleys/", "temp", ".zip"), 
                recursive = TRUE)
}

# DL Espace_Vert.shp
dl_unzip("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/download/shapefile.zip")
# DL ruelles-vertes.shp
dl_unzip("https://data.montreal.ca/dataset/ab3ce7bb-09a7-49d7-8f76-461ed4c39937/resource/e1440534-f438-43d3-ab7b-bcd09d72d3cd/download/ruelles-vertes.zip")

# Tidy and transform data -------------------------------------------------

green_space <- 
  read_sf("dev/data/green_alleys/Espace_Vert.shp") %>% 
  select(ID = OBJECTID, name = Nom, type_1 = TYPO1, type_2 = TYPO2, geometry) %>% 
  st_transform(4326) %>% 
  st_set_agr("constant") %>% 
  st_cast("POLYGON") %>% 
  mutate(visited = F)

alleys_mtl <-
  read_sf("dev/data/green_alleys/ruelles-vertes.shp") %>%
  st_transform(32618) %>%
  count(RUELLE_ID, DATE_AMENA) %>%
  rename(name = RUELLE_ID, date = DATE_AMENA) %>%
  group_by(name) %>%
  summarize(date = min(date[n == max(n)])) %>%
  st_transform(4326) %>%
  mutate(date = as.Date(date, "%Y%m%d")) %>% 
  mutate(visited = F)

# File from Google Earth
alleys_google <-
  read_sf("dev/data/green_alleys/RV_OnlyGM.shp") %>%
  mutate(date = as.Date(NA)) %>% 
  select(name = Name, date, geometry) %>%
  st_transform(32618) %>% 
  st_buffer(., 2) %>%
  st_transform(4326) %>% 
  mutate(visited = F)

# File from Montreal Nord
alleys_mn <-
  read_sf("dev/data/green_alleys/mt_nordrv.shp") %>%
  mutate(date = as.Date(NA)) %>% 
  select(name = Name, date) %>% 
  st_transform(32618) %>% 
  st_buffer(., 2) %>%
  st_transform(4326) %>% 
  mutate(visited = F)

# Alleys visited and info about them
alleys_visited <- 
  read_sf("dev/data/green_alleys/rv_visited.shp") %>%
  mutate(name = str_remove(Name, "^\\d*\\."),
         name = str_trim(name)) %>% 
  select(name)

alleys_visited_text <- 
  read_delim("dev/data/green_alleys/alleys_visited.csv", ";") %>%
  mutate(Photo_id = str_remove(Photo_id, "\\\n.*$")) |> 
  set_names(c("name", "type", "created", "description", "photo_ID")) %>% 
  mutate(description = str_replace_all(description, "<b>|</b>|<p>|\n", " ")) %>% 
  mutate(circulation = str_remove_all(description, ".*(?<=Circulation:)"),
         circulation = str_trim(circulation),
         description = str_remove_all(description, "(?>Circulation:).*|.*Description:"),
         description = str_trim(description)) %>%
  mutate(type = str_to_lower(type)) %>% 
  mutate_all(list(~na_if(.,"")))

alleys_visited <- 
alleys_visited %>%
  left_join(alleys_visited_text, ., by = "name") %>% 
  st_as_sf() %>% 
  st_transform(32618) %>% 
  st_buffer(., 2) %>%
  st_transform(4326) %>% 
  mutate(visited = T)

# Combine files ----------------------------------------

# First, give priority to the alleys_visited version
alleys <- 
  bind_rows(alleys_mtl, alleys_google, alleys_mn)

alleys_to_filter <- 
  alleys_visited %>% 
  st_intersection(alleys)

alleys <- 
  alleys %>% 
  filter(!name %in% alleys_to_filter$name.1)

alleys <- 
alleys_visited %>%
  bind_rows(alleys) %>% 
  mutate(ID = row_number()) %>% 
  select(ID, name, date, created, visited, type, description, circulation, photo_ID, geometry)

# Join borough name and CSDUID
alleys <- 
  alleys %>%
  st_join(rename(select(borough, ID, name), CSDUID = ID, name_2 = name)) %>%
  relocate(CSDUID, .after = name) %>% 
  relocate(name_2, .after = name)


# Get borough text --------------------------------------------------------

alley_text <-
  read_csv2("dev/data/green_alleys/info_borough.csv") %>%
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

alley_text <- 
alley_text %>% 
  mutate(first_alley = str_extract(description, "(?<=(created in:</b>)|(created:</b>))\\s*\\d*"),
         app_process = str_extract(description, "(?<=(Application process:</b>)|(Applicatin process:</b>)).*(?=<p>)"),
         management = str_extract(description, "(?<=Management:</b>).*(?=<p>)"),
         budget = str_extract(description, "(?<=Budget:</b>).*(?=<p>)"),
         contact = str_extract(description, "(?<=Contact:</b>).*"),
         guide = str_extract(description, "(?<=guide\">).*(?=</a>)")) %>%
  mutate_all(list(~str_trim(.))) %>% 
  mutate(first_alley = as.numeric(first_alley)) %>% 
  select(-description)

# Clean up ----------------------------------------------------------------

rm(alleys_mtl, alleys_google, alleys_mn, alleys_visited,
   alleys_to_filter, alleys_visited_text)
