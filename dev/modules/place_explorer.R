#### Place explorer data setup #################################################

# This script relies on objects created in dev/census.R


# Import postal codes -----------------------------------------------------

postal_codes <- 
  read_csv("dev/data/ZipCodeFiles/CanadianPostalCodes202103.csv")

postal_codes <- 
  postal_codes |>  
  filter(PROVINCE_ABBR == "QC") |>
  select(-PROVINCE_ABBR, -TIME_ZONE) |> 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |> 
  setNames(c("postal_code", "city", "geometry")) |> 
  st_filter(borough) |> 
  as_tibble() |> 
  st_as_sf() |> 
  mutate(postal_code = str_remove(str_to_lower(postal_code), "\\s"))
  