#### Reverse geocode building centroids ########################################
# Dependent script: needs 'building' object

# This script makes thousands of expensive calls to Google's geocoding API, so
# the geocoding portion should only be run a single time!

# Geocode with frequent saves ---------------------------------------------

# library(ggmap)
# 
# building <- qread("dev/data/building.qs", nthreads = future::availableCores())
# 
# address <-
#   building %>%
#   transmute(ID, name = NA_character_,)
# 
# centroids <- 
#   building |> 
#   st_transform(32618) |> 
#   st_centroid() |> 
#   st_transform(4326) |> 
#   pull(geometry)
# 
# for (i in seq_along(building$ID)) {
# 
#   print(i)
#   if (is.na(address[i,]$name)) address[i,]$name <- revgeocode(centroids[[i]])
#   if (i %% 1000 == 0) qsave(address, "dev/data/building_geocode.qs",
#                             nthreads = future::availableCores())
# 
# }
# 
# qsave(address, "dev/data/building_geocode.qs", 
#       nthreads = future::availableCores())
# rm(i, centroids)


address <- qread("dev/data/building_geocode.qs",
                 nthreads = future::availableCores())


# Parse results -----------------------------------------------------------

names <-
  address |> 
  mutate(name = str_remove(name, ", QC.*$")) |>
  mutate(name = if_else(str_detect(name, ", .*,"), str_remove(name, "^[^,]*, "),
                        name)) |> 
  pull(name)


# Join building to geocode results -----------------------------------------

building <- 
  building |> 
  mutate(name = names, .after = ID) |> 
  st_set_agr("constant")

rm(address, names)


# Temporarily trim to just island -----------------------------------------

island_CSDUID <- 
  c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
    "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
    "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
    "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
    "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
    "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
    "2466142", "2466072", "2466023")

building <- 
  building |> 
  filter(CSDUID %in% island_CSDUID)

rm(island_CSDUID)


# Create DA geometries and join to DA -------------------------------------

# Union by DA
building_DA <- 
  building |> 
  st_transform(32618) |> 
  group_by(DAUID) |> 
  summarize() |> 
  st_transform(4326)

DA <- 
  building_DA |> 
  as_tibble() |> 
  rename(ID = DAUID, building = geometry) |> 
  right_join(DA, by = "ID") |> 
  mutate(building = st_sfc(map2(building, geometry, ~{
    if (st_is_empty(.x)) .y else .x
  }))) |> 
  st_as_sf(crs = 4326) |> 
  relocate(building, .before = geometry) |> 
  st_set_geometry("geometry") |> 
  arrange(ID) |> 
  st_set_agr("constant")

rm(building_DA)
