# Import DA, CT and borough geometries ------------------------------------
# Independent script

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(qs)
  library(cancensus)
  })

# Variables to be selected from get_census
var_select <- c("CTUID" = "CT_UID", "CSDUID" = "CSD_UID", "name" = "name",
                "population" = "Population", "households" = "Households")

# Download DAs
CDs <- get_census(region = region, crs = crs, scale = "CD", format = FALSE)$ID
DA <- get_census(region = list(CD = CDs), scale = "DA")

# Download DB for the city
DB <- get_census(region = list(CSD = 2466023), scale = "DB")

# Download CTs, fill in with CSDs
CT <- get_census(region = region, scale = "CT")

csds <- 
  get_census(region = region, scale = "CSD") |> 
  st_transform(crs)

csds_buffered <- 
  csds |> 
  mutate(geometry = st_buffer(geometry, -25))

filling_CTs <- csds[-{
    st_intersects(st_transform(CT, crs), csds_buffered) |> 
    unlist() |> 
    unique()}, ] |> 
  mutate(CSDUID = ID) |> 
  st_transform(4326)

CT <- rbind(CT, filling_CTs)

# Download CSDs
CSD <-
  cancensus::get_census("CA16", list(CMA = cma), "CSD", geo_format = "sf",
                        quiet = TRUE) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(ID = GeoUID, any_of(var_select), geometry) |> 
  arrange(ID) |> 
  mutate(type = "City", .after = name) |> 
  mutate(name = str_remove(name, " \\(.*\\)")) |> 
  st_set_agr("constant")

rm(var_select, get_census, csds, filling_CTs, CDs, csds_buffered)


# Add borough/CSD names ---------------------------------------------------

CSD <- 
  CSD |> 
  rename(name_2 = type) |> 
  st_set_agr("constant")

CT <- 
  CT |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = ID, name_2 = name),
            by = "CSDUID") |>
  relocate(name_2, .after = name) |>
  mutate(CTUID = ID, .after = name_2) |>
  st_set_agr("constant")

DA <- 
  DA |> 
  left_join(select(st_drop_geometry(CSD), CSDUID = ID, name_2 = name), 
            by = "CSDUID") |>
  relocate(name_2, .after = name) |> 
  mutate(CTUID = if_else(is.na(CTUID), CSDUID, CTUID)) |> 
  mutate(DAUID = ID, .after = name_2) |> 
  st_set_agr("constant")

# + add DAUID to DB
DB_intersects_DA <- st_intersects(st_centroid(DB), DA)
DB$DAUID <- map_chr(seq_len(nrow(DB)), ~{DA[unlist(DB_intersects_DA)[.x], ]$ID})
DB <- 
  DB |> 
  select(-CSDUID) |> 
  left_join(select(st_drop_geometry(DA), CSDUID, DAUID), by = "DAUID") |> 
  left_join(st_drop_geometry(borough) |> 
              select(CSDUID, name_2 = name), 
            by = "CSDUID") |> 
  relocate(name_2, .after = name) |> 
  relocate(DAUID, .after = name_2) |> 
  relocate(CSDUID, .after = CTUID) |> 
  mutate(DBUID = ID, .after = name_2) |> 
  st_set_agr("constant")


rm(borough, DB_intersects_DA)
