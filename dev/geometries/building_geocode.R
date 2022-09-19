#### Reverse geocode building centroids ########################################
# Dependent script: needs 'building' object

# This script makes thousands of calls to OSM geocoding API, so
# the geocoding portion should only be run a single time!

library(future)
library(progressr)
library(furrr)
plan(multisession)

# Bind address if found previously ----------------------------------------

old_building <- 
  qread("data2/building_full.qs") |> 
  transmute(old_ID = as.numeric(ID), name) |>
  arrange(old_ID) |> 
  st_transform(32618)

new_centroids <-
  building |>
  st_transform(32618) |>
  st_centroid()

same_building <- st_intersects(new_centroids, old_building)

building$old_ID <- 
  map_int(same_building, ~{if (length(.x[1]) == 0) NA else .x[1]})

building <- 
  building |> 
  left_join(st_drop_geometry(old_building), by = "old_ID") |> 
  relocate(name, .before = name_2) |> 
  select(-old_ID)


# Geocode with frequent saves ---------------------------------------------

centroids <-
  building |> 
  filter(is.na(name)) |> 
  select(ID) |> 
  st_transform(32618) |>
  st_centroid() |> 
  st_transform(4326) |>
  mutate(lat = st_coordinates(geometry)[,"X"],
         lon = st_coordinates(geometry)[,"Y"])

reverse_geocode <- function(lat, lon) {
  paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", 
                  lat, "&lon=", lon, "&addressdetails=1") |> 
             httr::GET(.random_proxy(), httr::timeout(2)) |> 
             httr::content() |> 
             pluck("address")
}

centroids$group <- cut(seq_len(nrow(centroids)), 250)
centroids_up <- centroids
dir.create("dev/data/building_geocode_lists")

with_progress({
  handlers(list(
    handler_progress(
      format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 60,
      complete = "="
    )
  ))

  # Do not retrieve twice the same address, if the loop must be terminated
  all_buildings <- 
    list.files("dev/data/building_geocode_lists", full.names = TRUE) |> 
    map(qread) |> 
    reduce(c)
  centroids_up <- 
    centroids |> 
    filter(!ID %in% names(all_buildings))
  unlink(list.files("dev/data/building_geocode_lists", full.names = TRUE))
  qsave(all_buildings, 
        file = paste0("dev/data/building_geocode_lists/already_found.qs"))
  
  pb <- progressor(nrow(centroids_up))
  
  # Map over all the groups
  imap(unique(centroids_up$group), function(group, it) {
    
    ids <- centroids_up[centroids_up$group == group, ]$ID
    
    out <- future_map(set_names(ids), function(id) {
      pb()
      to_rev <- centroids_up[centroids_up$ID == id, ]
      tryCatch(reverse_geocode(lon = to_rev$lat, lat = to_rev$lon),
               error = function(e) {
                 print(e)
                 return(NA)})
    })
    
    qsave(out, file = paste0("dev/data/building_geocode_lists/", it, ".qs"))
  })
})

all_buildings <- 
  map(list.files("dev/data/building_geocode_lists", full.names = TRUE), qread) |> 
  reduce(c)

qsave(all_buildings, file = "dev/data/building_geocode.qs")

unlink("dev/data/building_geocode_lists")


# Open data and arrange it in a df ----------------------------------------

all_buildings <- qread("dev/data/building_geocode.qs")

imap_dfr(all_buildings, function(li, id) {
  tibble(ID = id,
         house_number = li$house_number,
         road = li$road,
         town = li$town,
         city = li$city,
         county = li$county,
         neighbourhood = li$neighbourhood,
         village = li$village,
         region = li$region)
})


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
