#### Reverse geocode building centroids ########################################
# Dependent script: needs 'building' object

# This script makes thousands of expensive calls to Google's geocoding API, so
# the geocoding portion should only be run a single time!

library(qs)


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
  mutate(name = if_else(str_detect(name, ", .*,"),
                        str_remove(name, "^[^,]*, "),
                        name)) |> 
  pull(name)


# Join building to geocode results -----------------------------------------

building <- 
  building |> 
  mutate(name = names, .after = ID)

rm(address, names)


# Temporarily trim to just central city -----------------------------------

downtown <- 
  borough |> 
  slice(c(33, 37, 45, 46, 47, 52))

building <- 
  building |> 
  st_transform(32618) |> 
  st_filter(st_transform(downtown, 32618)) |> 
  st_transform(4326) |> 
  st_set_agr("constant")

rm(downtown)

# To save output, run dev/build_data.R, which calls this script
