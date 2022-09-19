#### Reverse geocode building centroids ########################################
# Dependent script: needs 'building' object

# This script makes thousands of calls to OSM geocoding API, so
# the geocoding portion should only be run a single time!

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

address <-
  building |> 
  select(ID, name)

centroids <-
  address |>
  st_transform(32618) |>
  st_centroid() |> 
  st_transform(4326) |>
  mutate(lat = st_coordinates(geometry)[,"X"],
         lon = st_coordinates(geometry)[,"Y"])

reverse_geocode <- function(lat, lon) {
  
  paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", 
         centroids$lon[i], "&lon=", centroids$lat[i], "&addressdetails=1") |> 
    httr::GET() |> 
    httr::content() |> 
    pluck("address")
  
}

for (i in seq_along(centroids$ID)) {
  if (is.na(address[i,]$name)) {
    start <- Sys.time()
    
    z <- 
      paste0("https://nominatim.openstreetmap.org/reverse?format=xml&lat=", 
             centroids$lon[i], "&lon=", centroids$lat[i], "&addressdetails=1") |> 
      httr::GET() |> 
      httr::content() |> 
      rvest::html_element("result") |> 
      rvest::html_text()
    
    message('\r', paste0(i, "/", nrow(centroids), "  Last found: ", z), 
            appendLF = FALSE)
    
    address$name[i] <- z
    
    end <- Sys.time()
    # Respect the 1 request per second
    if (as.numeric(end - start) < 1) Sys.sleep(1 - as.numeric(end - start))
  }
  if (i %% 1000 == 0) qsave(address, "dev/data/building_geocode.qs",
                            nthreads = future::availableCores())
}

address <- qread("dev/data/building_geocode.qs",
                 nthreads = future::availableCores())


# Parse results -----------------------------------------------------------

names <-
  address |> 
  mutate(name = str_remove(name, ", [^,]*, [^,]*, [^,]*, [^,]*$")) |> 
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
