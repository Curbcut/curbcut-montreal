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

# Get census function
get_census <- function(re = list(PR = "24"), sc = scale, format = TRUE) {
  out <- cancensus::get_census(
    dataset = "CA16",
    regions = re,
    level = sc,
    geo_format = "sf",
    quiet = TRUE)  |> 
    as_tibble() |>
    st_as_sf() |>
    select(ID = GeoUID, any_of(var_select), geometry) |>
    arrange(ID) |>
    mutate(name = ID, .after = ID) |> 
    st_set_agr("constant")
  
  if (!format) return(out)
  
  keep_ids <- 
    out |>
    mutate(previous_area = units::drop_units(st_area(geometry))) |>
    st_intersection(master_polygon) |>
    st_set_agr("constant") |> 
    st_make_valid() |> 
    mutate(new_area = units::drop_units(st_area(geometry))) |>
    filter({new_area / previous_area} > 0.33) |>
    pull(ID)
  
  out |> 
    filter(ID %in% keep_ids)
}

# Download DAs
CDs <- get_census(sc = "CD", format = FALSE)$ID
DA <- get_census(re = list(CD = CDs), sc = "DA")

# Download CTs, fill in with CSDs
CT <- get_census(sc = "CT")

csds <- 
  get_census(sc = "CSD") |> 
  st_transform(32618)
csds_buffered <- 
  csds |> 
  mutate(geometry = st_buffer(geometry, -25))

filling_CTs <- csds[-{
    st_intersects(st_transform(CT, 32618), csds_buffered) |> 
    unlist() |> 
    unique()}, ] |> 
  mutate(CSDUID = ID) |> 
  st_transform(4326)

CT <- rbind(CT, filling_CTs)

# Download CSDs
CSD <-
  cancensus::get_census("CA16", list(CMA = "24462"), "CSD", geo_format = "sf", 
             quiet = TRUE) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(ID = GeoUID, any_of(var_select), geometry) |> 
  arrange(ID) |> 
  filter(name != "Montréal (V)") |> 
  mutate(type = "City", .after = name) |> 
  mutate(name = str_replace_all(name, "\\(PE\\)", "--parish municipality")) |> 
  mutate(name = str_remove(name, " \\(.*\\)")) |> 
  st_set_agr("constant")

rm(var_select, get_census, csds, filling_CTs, CDs)



# Clip boroughs -----------------------------------------------------------

# Get CMA boundary for clipping boroughs
CMA <- 
  get_census("CA16", list(CMA = "24462"), geo_format = "sf", quiet = TRUE) |> 
  st_set_agr("constant")

# Import boroughs from City and clip to CMA geometry
borough <-
  read_sf("dev/data/montreal_boroughs_2019.shp") |> 
  st_set_agr("constant") |> 
  st_transform(32618) |> 
  st_intersection(st_transform(CMA, 32618)) |> 
  st_transform(4326) |> 
  select(name = NOM, type = TYPE, geometry) |> 
  mutate(type = if_else(type == "Arrondissement", "Borough", "City")) |> 
  st_cast("MULTIPOLYGON")

# Get CSDs whose geometries will be replaced with borough geometries
replacements <-
  CSD |> 
  st_transform(32618) |> 
  st_centroid() |> 
  st_join(select(st_transform(borough, 32618), new_name = name),
          left = FALSE) |> 
  st_drop_geometry() |> 
  inner_join(borough, by = c("new_name" = "name")) |> 
  select(-name, -type.x) |> 
  rename(name = new_name, type = type.y) |> 
  relocate(name, type, .after = ID)

# Replace geometries
CSD <- 
  CSD |> 
  filter(!ID %in% replacements$ID) |> 
  bind_rows(replacements) |> 
  arrange(ID) |> 
  mutate(CSDUID = ID, .after = name)

# Filter borough to remaining cases
borough <- 
  borough  |>  
  filter(!name %in% replacements$name)

rm(CMA, replacements)

# Join DAs to remaining boroughs by centroid
borough_join <-
  DA |> 
  filter(CSDUID == "2466023") |> 
  select(ID, geometry) |> 
  st_transform(32618) |> 
  st_centroid(of_largest_polygon = TRUE) |> 
  st_join(st_transform(borough, 32618), left = FALSE) |> 
  select(ID, name) |> 
  st_drop_geometry()

# Find any DAs in Montréal which didn't join
leftovers <-
  DA |> 
  filter(!ID %in% borough_join$ID) |> 
  filter(CSDUID == "2466023")

# If there is just one leftover, manually add it to Ahuntsic-Cartierville
if (nrow(leftovers) == 1) {
  borough_join <- 
    leftovers |> 
    st_drop_geometry() |> 
    select(ID) |> 
    mutate(name = "Ahuntsic-Cartierville") |> 
    bind_rows(borough_join)
}

# Redo data processing for DAs in borough_join
borough <-
  DA |> 
  select(-name) |> 
  st_drop_geometry() |> 
  inner_join(borough_join, by = "ID") |> 
  group_by(CSDUID, name) |> 
  summarize(across(c(population:households), sum, na.rm = TRUE), 
            .groups = "drop") |> 
  right_join(borough, by = "name") |> 
  relocate(type, CSDUID, .after = name) |> 
  mutate(ID = paste0(CSDUID, "_", seq_along(name)), .before = name) |> 
  st_as_sf() |> 
  bind_rows(CSD) |> 
  arrange(ID) |> 
  st_set_agr("constant")

# Update CSDUID in borough, DA and CT
borough_join <- 
  borough |> 
  st_drop_geometry() |> 
  select(ID, name) |> 
  right_join(borough_join, by = "name") |> 
  select(ID = ID.y, CSDUID_new = ID.x) |> 
  arrange(ID)

borough <- 
  borough |> 
  mutate(CSDUID = ID, .after = name)

DA <- 
  DA |> 
  left_join(borough_join, by = "ID") |> 
  mutate(CSDUID = coalesce(CSDUID_new, CSDUID)) |> 
  select(-CSDUID_new)

CT <-
  DA |> 
  st_drop_geometry() |> 
  select(ID = CTUID, CSDUID_new = CSDUID) |> 
  distinct() |> 
  filter(str_detect(CSDUID_new, "_")) |> 
  group_by(ID) |> 
  # Manual fix for Pierrefonds
  slice(1) |> 
  ungroup() |> 
  right_join(CT, by = "ID") |> 
  mutate(CSDUID = coalesce(CSDUID_new, CSDUID)) |> 
  select(-CSDUID_new) |> 
  st_as_sf()

rm(borough_join, CSD, leftovers)


# Add borough/CSD names ---------------------------------------------------

borough <- 
  borough |> 
  rename(name_2 = type) |> 
  st_set_agr("constant")

CT <- 
  CT |> 
  left_join(select(st_drop_geometry(borough), CSDUID = ID, name_2 = name),
            by = "CSDUID") |>
  relocate(name_2, .after = name) |>
  mutate(CTUID = ID, .after = name_2) |>
  st_set_agr("constant")

DA <- 
  DA |> 
  left_join(select(st_drop_geometry(borough), CSDUID = ID, name_2 = name), 
            by = "CSDUID") |>
  relocate(name_2, .after = name) |> 
  mutate(CTUID = if_else(is.na(CTUID), CSDUID, CTUID)) |> 
  mutate(DAUID = ID, .after = name_2) |> 
  st_set_agr("constant")
