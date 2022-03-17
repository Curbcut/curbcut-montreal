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
  st_as_sf(crs = 4326) |> 
  relocate(building, .before = geometry) |> 
  st_set_geometry("geometry") |> 
  arrange(ID) |> 
  st_set_agr("constant")

rm(building_DA)
