#### Green alley data setup #################################################### 

# This script relies on objects created in dev/census.R

# Get updated data from open data portal ----------------------------------

# dl_unzip <- function(shp_url, name) {
#   download.file(shp_url, destfile = paste0("dev/data/green_alleys/", "temp", 
#                                            ".zip"))
#   
#   unzip(paste0("dev/data/green_alleys/", "temp", ".zip"),
#         exdir = "dev/data/green_alleys")
#   
#   unlink(paste0("dev/data/green_alleys/", "temp", ".zip"), recursive = TRUE)
# }
# 
# # DL Espace_Vert.shp
# dl_unzip(paste0("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-",
#                 "565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/",
#                 "download/shapefile.zip"))
# 
# # DL ruelles-vertes.shp
# dl_unzip(paste0("https://data.montreal.ca/dataset/ab3ce7bb-09a7-49d7-8f76-",
#                 "461ed4c39937/resource/e1440534-f438-43d3-ab7b-bcd09d72d3cd/",
#                 "download/ruelles-vertes.zip"))
# 
# rm(dl_unzip)


# Tidy and transform data -------------------------------------------------

green_space <- 
  read_sf("dev/data/green_alleys/Espace_Vert.shp") |>
  select(ID = OBJECTID, name = Nom, type_1 = TYPO1, type_2 = TYPO2, geometry) |>
  st_transform(4326) |> 
  st_set_agr("constant") |>
  st_cast("POLYGON") |> 
  mutate(visited = FALSE, .after = type_2)

alleys_mtl <-
  read_sf("dev/data/green_alleys/ruelles-vertes.shp") |>
  st_transform(32618) |>
  count(RUELLE_ID, DATE_AMENA) |>
  rename(name = RUELLE_ID, date = DATE_AMENA) |>
  group_by(name) |>
  summarize(date = min(date[n == max(n)])) |>
  st_transform(4326) |>
  mutate(date = as.Date(date, "%Y%m%d")) |> 
  mutate(visited = FALSE, .after = date)

# File from Google Earth
alleys_google <-
  read_sf("dev/data/green_alleys/RV_OnlyGM.shp") |>
  mutate(date = as.Date(NA)) |> 
  select(name = Name, date, geometry) |>
  st_transform(32618) |> 
  (\(x) st_buffer(x, 2))() |>
  st_transform(4326) |> 
  mutate(visited = FALSE, .after = date)

# File from Montreal Nord
alleys_mn <-
  read_sf("dev/data/green_alleys/mt_nordrv.shp") |>
  mutate(date = as.Date(NA)) |> 
  select(name = Name, date) |> 
  st_transform(32618) |> 
  (\(x) st_buffer(x, 2))() |>
  st_transform(4326) |> 
  mutate(visited = FALSE, .after = date)

# Alleys visited and info about them
alleys_visited <- 
  read_sf("dev/data/green_alleys/rv_visited.shp") |>
  st_zm() |>
  mutate(name = str_remove(Name, "^\\d*\\."),
         name = str_trim(name)) |> 
  select(name)

alleys_visited_text <-
  suppressMessages(read_csv2("dev/data/green_alleys/alleys_visited.csv", 
            show_col_types = FALSE)) |>
  mutate_all(list(~na_if(.,"")))

# Which photo does not exist? Throw a warning message
missing_photos <- 
  alleys_visited_text[!file.exists(
    paste0("www/alleys/", alleys_visited_text$photo_ID)),] |> 
  pull(photo_ID)

if (length(missing_photos > 0)) {
  warning(paste0(
    "Missing green alley photo: 'www/alleys/", missing_photos, "'\n"))
  
  alleys_visited_text <- 
    alleys_visited_text |> 
    mutate(photo_ID = ifelse(photo_ID %in% missing_photos, NA, photo_ID))
}

alleys_visited <- 
  alleys_visited |>
  left_join(alleys_visited_text, ., by = "name") |> 
  st_as_sf() |> 
  st_transform(32618) |> 
  st_buffer(2) |>
  st_transform(4326) |> 
  mutate(visited = TRUE, .after = circulation) |>
  st_set_agr("constant")


# Combine files -----------------------------------------------------------

# First, give priority to the alleys_visited version
alleys <- 
  bind_rows(alleys_mtl, alleys_google, alleys_mn) |> 
  st_set_agr("constant")

alleys_to_filter <- 
  alleys_visited |> 
  st_intersection(alleys)

alleys <- 
  alleys |> 
  filter(!name %in% alleys_to_filter$name.1)

alleys <- 
  alleys_visited |>
  bind_rows(alleys) |> 
  mutate(ID = row_number()) |> 
  select(ID, name, date, created, visited, type, description, circulation, 
         photo_ID, geometry)

# Join borough name and CSDUID
alleys <- 
  alleys |>
  st_join(rename(select(borough, ID, name), CSDUID = ID, name_2 = name)) |>
  relocate(CSDUID, name_2, .after = name)


# Get borough text --------------------------------------------------------
mtl_ids <- 
borough |> 
  st_drop_geometry() |> 
  select(ID, name) |> 
  filter(str_starts(ID, "2466023"))

alley_text <-
  suppressMessages(read_csv2("dev/data/green_alleys/info_borough.csv")) |> 
  # Attaching the right IDs
  select(-ID) |> 
  left_join(mtl_ids, by = "name") |> 
  relocate(ID, .before = name)

# Add total lengths of green alleys to boroughs
alleys_length <- 
alleys |> 
  mutate(green_alley_sqm = st_area(geometry)/2)

alley_text <- 
  alley_text |> 
  left_join(alleys_length |> 
               st_drop_geometry() |> 
               group_by(CSDUID) |>
               summarize(green_alley_sqm = round(units::drop_units(sum(green_alley_sqm, na.rm = T)))),
             by = c("ID" = "CSDUID")) |> 
  relocate(green_alley_sqm, .after = first_alley)


# Add green alleys sqm to census geographies -----------------------------

lengths_alleys_fun <- function(data) {
  
  sqm_per_id <- 
  alleys_length |> 
    rename(alley_ID = ID) |> 
    st_join(select(data, ID)) |> 
    st_drop_geometry() |> 
    group_by(ID) |> 
    summarize(green_alley_sqm = round(units::drop_units(sum(green_alley_sqm, na.rm = T))))
  
  sqm_per_id |> 
    left_join(select(data, ID, population), ., by = "ID") |> 
    st_as_sf() |> 
    mutate(green_alley_sqkm = 1000 * green_alley_sqm / units::drop_units(st_area(geometry)),
           green_alley_per1k =  1000 * green_alley_sqm / population) |> 
    select(-green_alley_sqm) |> 
    mutate(across(starts_with("green_alley"), ntile, n = 3, .names = "{.col}_q3"), 
           .before = geometry) |> 
    select(-population) |> 
    st_drop_geometry()

}

join_alleys <- 
  map(list("borough" = borough, "CT" = CT, "DA" = DA), lengths_alleys_fun)


# Data testing ------------------------------------------------------------

data_testing(join_alleys)


# Joining data to geometries ----------------------------------------------

borough <- left_join(borough, join_alleys$borough, by = "ID") |> 
  relocate(geometry, .after = last_col())

CT <- left_join(CT, join_alleys$CT, by = "ID") |> 
  relocate(geometry, .after = last_col())

DA <- left_join(DA, join_alleys$DA, by = "ID") |> 
  relocate(geometry, .after = last_col())


# Meta testing ------------------------------------------------------------

meta_testing()


# Clean up ----------------------------------------------------------------

rm(alleys_mtl, alleys_google, alleys_mn, alleys_visited, alleys_to_filter, 
   alleys_visited_text, missing_photos, alley_text, alleys_length,
   mtl_ids, lengths_alleys_fun, join_alleys)


# Variable explanations ---------------------------------------------------

variables <-
  variables |>
  add_variables(
    var_code = "green_alley_sqkm",
    var_title = "Green alleys per sq km",
    var_short = "Alleys sqkm",
    explanation = paste0("the number of square meters of green alley per ",
                         "square kilometers"),
    category = NA,
    private = FALSE,
    dates = "2021",
    scales = c("borough", "CT", "DA"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "VdM") |> 
  add_variables(
    var_code = "green_alley_per1k",
    var_title = "Green alleys per 1,000",
    var_short = "Alleys 1,000",
    explanation = paste0("the number of square meters of green alley per ",
                         "1,000 residents"),
    category = NA,
    private = FALSE,
    dates = "2021",
    scales = c("borough", "CT", "DA"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "VdM")
