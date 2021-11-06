#### Reverse geocode street edge centroids #####################################
# Dependent script: needs 'street' object

# This script makes thousands of expensive calls to Google's geocoding API, so
# the geocoding portion should only be run a single time!


# Identify new/old geometries for geocoding -------------------------------

address <- tryCatch(
  qread("dev/data/street_geocode.qs"), error = function(e) {
    street[0,] |> 
      mutate(name = character()) |> 
      select(ID, name, name_2, street_type, osm_ID, geometry)})

# if (nrow(address) != nrow(street)) {
#   
#   to_geocode <- 
#     street |> 
#     anti_join(as_tibble(address), 
#               by = c("name_2", "street_type", "osm_ID", "geometry")) |> 
#     select(ID, name_2, street_type, osm_ID, geometry) |> 
#     mutate(name = NA_character_, .after = ID)
#   
# }
# 
# 
# # Geocode with frequent saves ---------------------------------------------
# 
# if (exists("to_geocode") && nrow(to_geocode) > 0) {
#   
#   suppressPackageStartupMessages({library(ggmap)})
#   
#   centroids <- 
#     to_geocode |> 
#     st_transform(32618) |> 
#     st_centroid() |> 
#     st_transform(4326) |> 
#     pull(geometry)
#   
#   for (i in seq_along(to_geocode$ID)) {
#     
#     print(i)
#     to_geocode[i,]$name <- revgeocode(centroids[[i]])
#     if (i %% 100 == 0) qsave(to_geocode, "dev/data/street_geocode_temp.qs")
#     
#   }
#   
#   address <- bind_rows(address, to_geocode)
#   
#   qsave(address, "dev/data/street_geocode.qs")
#   unlink("dev/data/street_geocode_temp.qs")
#   rm(i, centroids, to_geocode)
#   
# }


# Apply geocoding results to street ---------------------------------------

street <- 
  street |> 
  left_join(
    address |> 
      as_tibble() |> 
      select(name, name_2, street_type, osm_ID, geometry),
    by = c("name_2", "street_type", "osm_ID", "geometry")) |> 
  relocate(name, .after = ID)

rm(address)


# Parse results -----------------------------------------------------------

street <- 
  street |> 
  mutate(name = str_remove(name, ", QC.*$")) |>
  mutate(name = if_else(str_detect(name, ", .*,"), str_remove(name, "^[^,]*, "),
                        name))
  
# To save output, run dev/build_geometries.R, which calls this script
