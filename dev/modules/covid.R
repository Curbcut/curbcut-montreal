#### COVID data setup ##########################################################

# Get data ----------------------------------------------------------------

# 2020 shapefiles
covid_may_2020 <- 
  read_sf("dev/data/COVID/corridors_sanitaires-may/corridors_sanitaires.shp") |> 
  select(ID = objectid, street = Rue, type = Type_corri, geometry) |> 
  mutate(timeframe = "may_2020", .before = geometry) |> 
  st_transform(4326) |> 
  st_set_agr("constant")

covid_july_2020 <- 
  read_sf("dev/data/COVID/corridors_sanitaires-july/corridors_sanitaires.shp") |> 
  select(ID = objectid, street = Rue, type = Type_corri, geometry) |> 
  mutate(timeframe = "july_2020", .before = geometry) |> 
  st_transform(4326) |> 
  st_set_agr("constant")

covid_oct_2020 <- 
  read_sf("dev/data/COVID/corridors_sanitaires-october/corridors_sanitaires.shp") |> 
  select(ID = objectid, street = Rue, type = Type_corri, geometry) |> 
  mutate(timeframe = "oct_2020", .before = geometry) |> 
  st_transform(4326) |> 
  st_set_agr("constant")

covid <- 
  rbind(covid_may_2020, covid_july_2020, covid_oct_2020)

# 2021 shapefile
# covid_2021 <- 
#   read_sf("dev/data/COVID/2021_covid_pedestrian/2021_pedestrian_streets.shp") |> 
#   select(ID = UNIQUEID, street = STREETNAME, geometry) |> 
#   mutate(type = "Corridor piéton élargi", .before = geometry) |>
#   st_transform(4326) |> 
#   st_set_agr("constant")

# Photos from 2020
covid_pics <- 
  read_sf("dev/data/COVID/2020covidphotos.kml") |>
  st_zm() |>
  mutate(ID = row_number(), .before = Name) |> 
  rename(street = Name, type = Description) |>
  mutate(type = as.character(type),
         type = str_remove_all(type, "<.*?>")) |> 
  mutate(feature_ID = case_when(
           ID == 1 ~ list(45),
           ID %in% c(2, 9) ~ list(50),
           ID %in% c(3, 4) ~ list(2629),
           ID %in% c(5, 6, 7) ~ list(2747),
           ID %in% c(8, 25, 29) ~ list(c(21, 22)),
           ID == 10 ~ list(c(2842, 2509, 2813, 2442, 2811)),
           ID %in% c(11, 12, 13) ~ list(2812),
           ID %in% c(14, 15) ~ list(2497),
           ID %in% 16 ~ list(2732),
           ID %in% 17 ~ list(2726),
           ID %in% 18 ~ list(2796),
           ID %in% 19 ~ list(c(2526, 2845)),
           ID %in% 20 ~ list(c(2492, 2617, 2652)),
           ID %in% c(21, 26) ~ list(24),
           ID %in% c(22, 30) ~ list(c(35, 36, 40)),
           ID %in% 23 ~ list(2601),
           ID %in% 24 ~ list(2620),
           ID %in% 27 ~ list(27),
           ID %in% 28 ~ list(2654),
           ID %in% c(31, 32) ~ list(2835)), 
         .before = geometry)

# Photo join table
photo_join <- 
  covid_pics |> 
  st_drop_geometry() |> 
  select(ID, feature_ID) |> 
  unnest(feature_ID) |> 
  group_by(feature_ID) |> 
  summarize(photo_ID = list(ID))

# Create colour table
covid_colours <- tibble(
  type = c("Circuit des voies actives et sécuritaires", 
           "Circulation locale", 
           "Corridor piéton élargi", 
           "Corridor projecté", 
           "File d'attente encadrée", 
           "Rue familiale et active", 
           "Rue fermée", 
           "Rue partiellement fermée"), 
  covid_colour = c("#FF5733FF", "#FFD733FF", "#5F940EFF", "#10A9A7FF",
                   "#2D80CAFF", "#FF7C2DFF", "#6F2094FF", "#75BB79FF"))


covid <- 
  covid |> 
  left_join(photo_join, by = c("ID" = "feature_ID")) |>
  mutate(fill = case_when(type == "Circuit des voies actives et sécuritaires" ~ "#FF5733FF",
                          type == "Circulation locale" ~ "#FFD733FF",
                          type == "Corridor piéton élargi" ~ "#5F940EFF",
                          type == "Corridor projeté" ~ "#10A9A7FF",
                          type == "File d'attente encadrée" ~ "#2D80CAFF",
                          type == "Rue familiale et active" ~ "#FF7C2DFF",
                          type == "Rue fermée" ~ "#6F2094FF",
                          type == "Rue partiellement fermée" ~ "#75BB79FF",
                          type == "Rue partagée" ~ "#75A7BAFF")) |> 
  relocate(photo_ID, fill, .before = geometry)


# Clean up ----------------------------------------------------------------

rm(photo_join, covid_colours, covid_may_2020, covid_july_2020, 
   covid_oct_2020)

### actual 2020 .png files are located in Sus/www/COVIDpics

