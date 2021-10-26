#### Reverse geocode street edge centroids #####################################
# Dependent script: needs 'street' object

# This script makes thousands of expensive calls to Google's geocoding API, so
# the geocoding portion should only be run a single time!


# Geocode with frequent saves ---------------------------------------------

library(ggmap)

address <-
  street %>%
  st_drop_geometry() |> 
  mutate(name = NA_character_, .before = name_2)

centroids <- 
  street |> 
  st_transform(32618) |> 
  st_centroid() |> 
  st_transform(4326) |> 
  pull(geometry)

for (i in seq_along(street$ID)) {

  print(i)
  address[i,]$name <- revgeocode(centroids[[i]])
  if (i %% 100 == 0) qsave(address, "dev/data/street_geocode.qs")

}

qsave(address, "dev/data/street_geocode.qs")
rm(i, centroids)


address <- qread("dev/data/street_geocode.qs")


# Parse results -----------------------------------------------------------

# Use municipality names to detect end of street address
city_list <- c(borough$name, "Montréal", "Montreal", "Baie-D'Urfe",
               "Pointe-aux-Trembles", "Lasalle", "Pierrefonds", "L'Île-Bizard",
               "Côte Saint-Luc", "Mount Royal", "Roxboro", "Sainte-Geneviève",
               "Riviere-des-Prairies—Pointe-aux-Trembles", "Montreal West",
               "L'Île-Bizard—Sainte-Geneviève", "Dollard-Des Ormeaux",
               "Notre-Dame-de-Grâce", "IL BIZARD")

# Extract street address from full address string
names <-
  addresses %>%
  pull(name) %>%
  str_remove(paste0(", (", paste(city_list, collapse = "|"), "), QC.*$"))

# Search for results still containing postal codes to apply manual fixes
names_to_fix <-
  names %>%
  str_which("[:upper:][:digit:][:upper:] [:digit:][:upper:][:digit:]")

names[names_to_fix] <- c(
  "1515 Rue Rancourt",
  "Senneville Migratory Bird Sanctuary",
  "Senneville Migratory Bird Sanctuary",
  "Senneville Migratory Bird Sanctuary",
  "Senneville Migratory Bird Sanctuary",
  "Senneville Migratory Bird Sanctuary",
  "Senneville Migratory Bird Sanctuary",
  "Angell Woods",
  "Bois-de-la-Roche Agricultural Park",
  "Bois-de-la-Roche Agricultural Park",
  "Bois-de-la-Roche Agricultural Park",
  "Baie Forget",
  "Bois-de-la-Roche Agricultural Park",
  "Bois-de-la-Roche Agricultural Park",
  "L'Anse-à-l'Orme",
  "L'Anse-à-l'Orme",
  "21 Chemin South Ridge",
  "Royal Montreal Golf Club",
  "4220 Rue de Rouen",
  "Lafarge quarry",
  "10200 Sherbrooke St E",
  "10605 Boul Henri-Bourassa E",
  "11355 Boul Henri-Bourassa E"
)


# Join street to geocode results -------------------------------------------

street <- 
  street %>% 
  mutate(name = names, .after = ID)

rm(addresses, city_list, names, names_to_fix)

# To save output, run dev/build_geometries.R, which calls this script
