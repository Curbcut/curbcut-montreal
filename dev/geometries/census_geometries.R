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
get_census <- function(region = list(PR = "24"), scale, format = TRUE, 
                       crs = 32618) {
  out <- cancensus::get_census(
    dataset = "CA16",
    regions = region,
    level = scale,
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
    st_transform(crs) |> 
    st_point_on_surface() |> 
    st_filter(st_transform(master_polygon, crs)) |> 
    pull(ID)
  
  out |> 
    filter(ID %in% keep_ids)
}

# Download DAs
CDs <- get_census(scale = "CD", format = FALSE)$ID
DA <- get_census(region = list(CD = CDs), scale = "DA")

# Download DB for the city
DB <- get_census(region = list(CSD = 2466023), scale = "DB")

# Download CTs, fill in with CSDs
CT <- get_census(scale = "CT")

csds <- 
  get_census(scale = "CSD") |> 
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

rm(var_select, get_census, csds, filling_CTs, CDs, csds_buffered)


# Clip boroughs -----------------------------------------------------------

# Get CMA boundary for clipping boroughs
CMA_boundaries <- 
  get_census("CA16", list(CMA = "24462"), geo_format = "sf", quiet = TRUE) |> 
  st_set_agr("constant")

# Import boroughs from City and clip to CMA geometry
borough <-
  read_sf("dev/data/montreal_boroughs_2019.shp") |> 
  st_set_agr("constant") |> 
  st_transform(32618) |> 
  st_intersection(st_transform(CMA_boundaries, 32618)) |> 
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

rm(CMA_boundaries, replacements)

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

rm(borough_join, CSD, leftovers)

CSD <- 
  borough |> 
  rename(name_2 = type) |> 
  st_set_agr("constant")


# Adding Laval ------------------------------------------------------------

# Adding Laval
laval <- st_read(paste0("dev/data/centraide/StatCan_Recensement2016/_Geograph",
                        "ie/Secteurs_damenagement_Ville_de_Laval.shp")) |> 
  sf::st_transform(4326) |> 
  transmute(name = gsub("Secteur \\d - ", "", Secteur), type = "Sector")

CSD <- susbuildr::split_scale(destination = CSD, 
                              cutting_layer = laval,
                              DA_table = DA,
                              crs = 32618)

CSD <- CSD |> 
  mutate(CSDUID = ID)

# Add borough/CSD names ---------------------------------------------------

CT_surface <- st_point_on_surface(st_transform(CT, 32618))

CT_index <- 
  CT_surface |> 
  select(-CSDUID) |> 
  st_intersection(select(st_transform(CSD, 32618), CSDUID, name) |> 
                    rename(name_2 = name)) |> 
  st_drop_geometry() |> 
  select(ID, CSDUID, name_2)

CT <- 
  CT |> 
  select(-CSDUID) |> 
  left_join(CT_index) |> 
  mutate(CTUID = ID) |> 
  relocate(name_2, CSDUID, CTUID, .after = name)

DA <- 
DA |> 
  select(-CSDUID) |> 
  left_join(st_drop_geometry(CT) |> 
              transmute(CTUID, CSDUID, name_2), by = "CTUID") |> 
  mutate(DAUID = ID) |> 
  relocate(name_2, CSDUID, CTUID, DAUID, .after = name)

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
