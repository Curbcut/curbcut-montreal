#### Access to amenities data setup ############################################


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(qs)


# Download data -----------------------------------------------------------

# # Get shapefiles from open data portals
# dl_unzip <- function(shp_url) {
#   download.file(shp_url, destfile = paste0("dev/data/amenity_access/", "temp",
#                                            ".zip"))
# 
#   unzip(paste0("dev/data/amenity_access/", "temp", ".zip"),
#         exdir = "dev/data/amenity_access/")
# 
#   unlink(paste0("dev/data/amenity_access/", "temp", ".zip"), recursive = TRUE)
# }
# 
# # DL Espace_Vert.shp - MTL
# dl_unzip(paste0("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d",
#                 "79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/downlo",
#                 "ad/shapefile.zip"))
#
# # DL ParcsEspacesVerts.shp - Longueuil
# dl_unzip(paste0("https://www3.longueuil.quebec/sites/longueuil/files/donnees_o",
#                 "uvertes/parcsespacesverts.zip"))
# 
# # DL Enseignement scolaire
# dl_unzip(paste0("https://www.donneesquebec.ca/recherche/dataset/2d3b5cf8-b347-",
#                 "49c7-ad3b-bd6a9c15e443/resource/2ae11c05-03b2-4006-bdb2-a49a4",
#                 "fa41c23/download/etablissements-meq-mes-esrishp.zip"))
# 
# # DL Service de garde
# download.file(url = paste0("https://www.donneesquebec.ca/recherche/dataset/be36f85e-e419",
#                 "-4978-9c34-cb5795622595/resource/89af3537-4506-488c-8d0e-6d8",
#                 "5b4033a0e/download/liste-des-services-de-garde-08.csv"),
#               destfile = "dev/data/amenity_access/daycare.csv")
# 
# # Geolocate daycares
# daycares <- 
#   read_csv("dev/data/amenity_access/daycare.csv")
# 
# # Must encode to latin1 as it's the right encoding for that file
# Encoding(daycares$REGION) <- "latin1"
# Encoding(daycares$ADRESSE) <- "latin1"
# Encoding(daycares$NOM_MUN_COMPO) <- "latin1"
# Encoding(daycares$NOM) <- "latin1"
# 
# daycares <-
#   daycares |>
#   filter(REGION %in% c("6 - Montréal", "15 - Laurentides",
#                        "14 - Lanaudière", "16 - Montérégie", "13 - Laval")) |> 
#   mutate(ADRESSE =
#            str_remove_all(ADRESSE,
#                           ", (bureau| bureau|rez-de-chaussée|AG-10|local|suite|appartement|porte) .*$") |>
#            str_remove_all("      \\de étage|, \\de étage") |>
#            str_remove_all("(?<=\\d)-\\d*|[A-Z](?=,)")) |>
#   mutate(ADRESSE = paste0(ADRESSE, ", ",NOM_MUN_COMPO, ", QC"))
# 
# # Geocode by postal code if geocoding did not work
# susmontreal_bbox <- read_sf("dev/data/susmontreal_bbox_5km.shp")
# postal_code <- 
#   read_csv("dev/data/ZipCodeFiles/CanadianPostalCodes202103.csv") |> 
#   filter(PROVINCE_ABBR == "QC") |>
#   select(-PROVINCE_ABBR, -TIME_ZONE) |> 
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |> 
#   setNames(c("postal_code", "city", "geometry")) |> 
#   st_filter(susmontreal_bbox) |> 
#   as_tibble() |> 
#   st_as_sf()
# 
# 
# daycares$geometry <- NULL
# 
# for (i in daycares$ADRESSE) {
# 
#   # In the case something in the loop fails and it has to be re-ran
#   if (!is.null(daycares$geometry[daycares$ADRESSE == i] |> unlist())) next
# 
#   new_geo <-
#     tryCatch(tmaptools::geocode_OSM(daycares$ADRESSE[daycares$ADRESSE == i], as.sf = TRUE,
#                            return.first.only = TRUE),
#              # If fails, get postal code's geolocation instead
#              error = function(e)       postal_code$geometry[
#                postal_code$postal_code ==
#                  daycares$CODE_POSTAL_COMPO[daycares$ADRESSE == i][[1]]]) |>
#     (\(x) if (is.null(x)) {
#       # If fails, get postal code's geolocation instead
#       postal_code$geometry[
#         postal_code$postal_code ==
#           daycares$CODE_POSTAL_COMPO[daycares$ADRESSE == i][[1]]]
#     } else x$point)()
# 
#   daycares$geometry[daycares$ADRESSE == i] <-
#     if (length(new_geo) == 0) st_sfc(st_point(), crs = 4326) else new_geo
# 
#   qsave(daycares, file = "dev/data/amenity_access/daycare.qs")
# 
# }
# 
# qsave(st_as_sf(daycares, crs = 4326), 
#       file = "dev/data/amenity_access/daycare.qs")
# 
# rm(dl_unzip)
# 
# Green spaces from OSM
# municipal_parks_osm <-
#   osmdata::opq(st_bbox(susmontreal_bbox), timeout = 200) |>
#   osmdata::add_osm_features(features = c("\"leisure\" = \"park\"")) |>
#   osmdata::osmdata_sf()
# 
# municipal_parks_osm <-
#   municipal_parks_osm$osm_multipolygons |>
#   st_cast("POLYGON") |>
#   select(osm_id) |>
#   rbind(municipal_parks_osm$osm_polygons[, "osm_id"])
# 
# municipal_parks_island_longueuil <-
#   read_sf("dev/data/amenity_access/Espace_Vert.shp") |>
#   st_transform(4326) |>
#   filter(TYPO1 %in% c("Parc d'arrondissement", "En cours de validation",
#                       "Grand parc", "Autre espace vert")) |>
#   select() |>
#   rbind(read_sf("dev/data/amenity_access/ParcsEspacesVerts.shp") |>
#           st_transform(4326) |> select())
# 
# municipal_parks_osm <-
#   municipal_parks_osm |>
#   st_filter(filter(borough, !str_starts(ID, "2466023") & name != "Longueuil"))
# 
# municipal_parks <-
#   rbind(municipal_parks_island_longueuil, select(municipal_parks_osm)) |>
#   st_cast("MULTIPOLYGON") |>
#   st_cast("POLYGON") |>
#   transmute(id = row_number())
# 
# qsave(municipal_parks, file = "dev/data/amenity_access/municipal_parks.qs")


# Load and clean data -----------------------------------------------------

# susmontreal 5km bbox
susmontreal_bbox <- read_sf("dev/data/susmontreal_bbox_5km.shp")

# Travel time matrix
tt_matrix_DA <- qread("dev/data/tt_matrix_DA.qs")

# Municipal parks
municipal_parks <- qread("dev/data/amenity_access/municipal_parks.qs")

# Food distribution
# Shapefile coming from McGill GéoIndex - Food Distribution
food_distribution <- 
  read_sf("dev/data/amenity_access/dmti_fooddistribution/dmti_fooddistribution_2021_p_point.shp") |> 
  transmute(id = g_objectid,
            name = str_to_title(g_name),
            sic = g_sic_1)

# Attach food distribution industry
sm_industry <- 
  food_distribution |> 
  distinct(sic)

sm_industry$industry <- 
  map_chr(sm_industry$sic, ~{
    rvest::read_html(
      paste0("https://www.naics.com/sic-industry-description/?code=", 
             str_extract(.x, "^\\d{4}"))) |> 
      rvest::html_elements("h6") |> 
      rvest::html_text() |> 
      pluck(1) |> 
      str_extract("(?<=\\d{4}—).*")
  })

food_distribution <- left_join(food_distribution, sm_industry, by = "sic")

# Filter out unhealthy industry
food_distribution <- 
  food_distribution |> 
  filter(industry != "Candy, Nut, and Confectionery Stores") |> 
  select(-sic)

# Health care
# Shapefile coming from McGill GéoIndex - Health care
health_care <- 
  read_sf("dev/data/amenity_access/dmti_healthcare/dmti_healthcare_2021_p_point.shp") |> 
  transmute(id = g_objectid,
            name = str_to_title(g_name),
            sic = g_sic_1) |> 
  st_transform(4326)

# Attach health care industry
sm_industry <- 
  health_care |> 
  distinct(sic)

sm_industry$industry <- 
  map_chr(sm_industry$sic, ~{
    rvest::read_html(
      paste0("https://www.naics.com/sic-industry-description/?code=", 
             str_extract(.x, "^\\d{4}"))) |> 
      rvest::html_elements("h6") |> 
      rvest::html_text() |> 
      pluck(1) |> 
      str_extract("(?<=\\d{4}—).*")
  })

health_care <- left_join(health_care, sm_industry, by = "sic")

# Filter out unrelated industry
health_care <- 
  health_care |> 
  filter(!industry %in% c("Medical Laboratories", 
                          "Civic, Social, and Fraternal Associations",
                          "Educational, Religious, and Charitable Trusts",
                          "Child Day Care Services")) |> 
  select(-sic)


# Schools
schools <- 
  list(public = read_sf("dev/data/amenity_access/PPS_Public_Ecole.shp"),
       private = read_sf("dev/data/amenity_access/PPS_Prive_Installation.shp") |> 
         rename(NOM_OFF_O = NOM_OFFCL)) |> 
  map(st_filter, susmontreal_bbox) |> 
  # The following is an example of a school in two buildings (Pool and gymnasium)
  # filter(OBJECTID %in% c(1987, 1988))
  map(~{
    .x |> 
      group_by(CD_ORGNS) |> 
      mutate(across(PRESC:ADULTE, ~sum(.x) |> min(1))) |> 
      ungroup()
  }) |> 
  map(distinct, CD_ORGNS, .keep_all = TRUE)

schools <- 
  imap_dfr(schools, function(data, z) {
    transmute(data,
              id = OBJECTID,
              name = NOM_OFF_O,
              preschool = PRESC,
              primary = PRIM,
              secondary = SEC,
              vocational = FORM_PRO,
              adult = ADULTE,
              public = if_else(z == "public", TRUE, FALSE))
  }) |> 
  arrange(id)


# Daycares
daycares <- 
  qread("dev/data/amenity_access/daycare.qs") |> 
  transmute(id = row_number(),
            spots_total = PLACE_TOTAL,
            reduced_contribution = if_else(is.na(SUBV), FALSE, TRUE))


# Municipal parks are polygons. Get the closest DA instead ----------------

DA_street_centroid <- qread("dev/data/pop_weighted_centroid_DA_street.qs")

municipal_parks$nearest_DA <- st_nearest_feature(municipal_parks, DA_street_centroid)

municipal_parks <-
  left_join(municipal_parks, DA_street_centroid |> 
              mutate(row = row_number()) |> 
              rename(DAUID = ID) |> 
              st_drop_geometry(),
            by = c("nearest_DA" = "row")) |> 
  select(id, DAUID) |> 
  mutate(sqkm = units::drop_units(st_area(geometry))/1e+6) |> 
  st_drop_geometry() |> 
  left_join(DA_street_centroid, by = c("DAUID" = "ID")) |> 
  st_as_sf() |> 
  select(id, sqkm)

# Number of amenities per DA ----------------------------------------------

old_plan <- future::plan()
future::plan(future::multisession)

point_amenities <- 
  list(food_distribution = food_distribution, 
       schools = schools,
       health_care = health_care)

# Join all amenities to a DA ID
DA_amenities <- 
  furrr::future_map(point_amenities, ~st_join(.x, select(DA, ID)))

# Get the amount of amenities reachable per mode in 15 minutes
DA_amenities <- 
  map(DA_amenities, function(amenity) {
    furrr::future_map(tt_matrix_DA, function(mode) {
      mode[mode$travel_time_p50 <= 15, ] |> 
        left_join(amenity, by = c("to_id" = "ID")) |> 
        filter(!st_is_empty(geometry)) |> 
        group_by(from_id) |> 
        summarize(amenities = n())
    })
  })

# Get amenities in one df
DA_amenities <- 
  furrr::future_imap(DA_amenities, function(amenity, n_amenity) {
    amenity |> 
      imap(~{
        names(.x)[2] <- paste0(n_amenity, "_", str_to_lower(.y), "_count")
        .x}) |> reduce(left_join, by = "from_id")
  })

# For parks, the amount of amenities reachable per mode in 15 minutes must
# be in sqkm
municipal_parks_sqkm <- st_join(municipal_parks, select(DA, ID))
DA_amenities$municipal_parks_sqkm <-
  furrr::future_imap(tt_matrix_DA, function(mode, n_mode) {
    out <- 
      mode[mode$travel_time_p50 <= 15, ] |> 
      left_join(municipal_parks_sqkm, by = c("to_id" = "ID")) |> 
      filter(!st_is_empty(geometry)) |> 
      group_by(from_id) |> 
      summarize(amenities = sum(sqkm))
    names(out)[2] <- paste0("municipal_parks_", str_to_lower(n_mode), "_sqkm")
    out
  }) |> reduce(left_join, by = "from_id")

# For daycares, the amount of amenities reachable per mode in 15 minutes must
# be per spots
daycares_spot <- st_join(daycares, select(DA, ID))
DA_amenities$daycares_spot <-
  furrr::future_imap(tt_matrix_DA, function(mode, n_mode) {
    out <- 
      mode[mode$travel_time_p50 <= 15, ] |> 
      left_join(daycares_spot, by = c("to_id" = "ID")) |> 
      filter(!st_is_empty(geometry)) |> 
      group_by(from_id) |> 
      summarize(amenities = sum(spots_total))
    names(out)[2] <- paste0("daycare_spots_", str_to_lower(n_mode), "_count")
    out
  }) |> reduce(left_join, by = "from_id")

future::plan(old_plan)

# Bind all to one dataframe
DA_amenities <- 
  reduce(DA_amenities, left_join, by = "from_id") |> 
  rename_with(~paste0("amenities_", .x)) |> 
  rename(ID = amenities_from_id)


# Get DA amenities at other scales ----------------------------------------
# On average, an individual living in this scale can reach X amenity in a 15 
# minutes trip

# Add all DAs, change NAs for 0s
DA_amenities <- list(DA = DA_amenities |> 
                       right_join(st_drop_geometry(select(DA, ID)), 
                                  by = "ID") |> 
                       mutate(across(where(is.numeric), ~
                                       ifelse(is.na(.x), 0, .x))))

# CT
DA_amenities$CT <-
  DA |> 
  st_drop_geometry() |> 
  select(ID, CTUID, population) |> 
  left_join(DA_amenities$DA, by = "ID") |> 
  group_by(CTUID) |>
  (\(x) summarize(x, across(3:(ncol(x) - 1), ~weighted.mean(.x, population, 
                                                            na.rm = TRUE))))() |> 
  rename(ID = CTUID)

# borough
DA_amenities$borough <-
  DA |> 
  st_drop_geometry() |> 
  select(ID, CSDUID, population) |> 
  left_join(DA_amenities$DA, by = "ID") |> 
  group_by(CSDUID) |>
  (\(x) summarize(x, across(3:(ncol(x) - 1), ~weighted.mean(.x, population, 
                                                            na.rm = TRUE))))() |> 
  rename(ID = CSDUID)

# centraide
DA_amenities$centraide <- 
  DA |> 
  select(ID, population) |> 
  # Some DAs are in more than one centraide neighbourhood. As DAs are very small
  # compared to centraide neighbourhood, it is good enough.
  st_join(transmute(centraide, centraide_ID = ID)) |> 
  st_drop_geometry() |> 
  select(ID, centraide_ID, population) |> 
  left_join(DA_amenities$DA, by = "ID") |> 
  group_by(centraide_ID) |>
  (\(x) summarize(x, across(3:(ncol(x) - 1), ~weighted.mean(.x, population, 
                                                            na.rm = TRUE))))() |> 
  rename(ID = centraide_ID)
  

# Calculate breaks --------------------------------------------------------

DA_amenities <- map(DA_amenities, add_q3)

DA_amenities_q3 <- map(DA_amenities, get_breaks_q3)
DA_amenities_q5 <- map(DA_amenities, get_breaks_q5)

DA_amenities <-
  map2(DA_amenities, DA_amenities_q5, 
       ~{bind_cols(.x, add_q5(.x, .y))})


# Add to variables table --------------------------------------------------

var_list <-
  DA_amenities$DA |>
  select(-ID, -contains(c("q3", "q5"))) |>
  names()

# Get breaks_q3
breaks_q3_active <-
  imap_dfr(DA_amenities_q3, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_active <-
  imap_dfr(DA_amenities_q5, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:5,
                                 .before = 1)})

# Add in variables table
new_rows <-
  map_dfr(var_list, function(var) {
    
    mode <- 
      case_when(str_detect(var, "_walk_") ~ "walk",
                str_detect(var, "_transit_") ~ "transit",
                str_detect(var, "_bicycle_") ~ "bicycle",
                str_detect(var, "_car_") ~ "car")
    
    amenity <- 
      case_when(str_detect(var, "_food_distribution_") ~ "food distributors",
                str_detect(var, "_schools_") ~ "schools",
                str_detect(var, "_daycare_spots_") ~ "daycare spots",
                str_detect(var, "_health_care_") ~ "health care facilities",
                str_detect(var, "_municipal_parks_") ~ 
                  "square kilometers of municipal parks")
    
    amenity_short <- 
      case_when(str_detect(var, "_food_distribution_") ~ "food",
                str_detect(var, "_schools_") ~ "schools",
                str_detect(var, "_daycare_spots_") ~ "daycare",
                str_detect(var, "_health_care_") ~ "health",
                str_detect(var, "_municipal_parks_") ~ "parks")
    
    pre <- 
      case_when(str_ends(var, "_count") ~ "the count",
                str_ends(var, "_sqkm") ~ "the number")
    
    explanation_ <- 
      if (mode == "walk") {
        glue::glue("{pre} of {amenity} accessible in a 15 minutes {mode}")
      } else {
        glue::glue("{pre} of {amenity} accessible in 15 minutes by {mode}")
      }
    
    # ADDED ROW
    out <-
      add_variables(variables,
                    var_code = var,
                    var_title = glue::glue("Accessibility to {amenity} by {mode}"),
                    var_short = glue::glue("Accessibility to {amenity_short} ({mode})"),
                    explanation = explanation_,
                    category = NA,
                    theme = "Accessibility to amenities",
                    private = TRUE,
                    dates = NA,
                    scales = c("DA", "CT", "borough", "centraide"),
                    breaks_q3 = select(breaks_q3_active,
                                       scale, date, rank, 
                                       var = all_of(var)),
                    breaks_q5 = select(breaks_q5_active,
                                       scale, date, rank, 
                                       var = all_of(var)),
                    source = "Centraide",
                    interpolated = list(c(DA = FALSE,
                                          CT = FALSE,
                                          borough = FALSE,
                                          centraide = FALSE)),
                    grouping = as.character(glue::glue("Accessibility to {amenity}")),
                    group_diff = as.character(glue::glue("By {mode}")))
    
    out[out$var_code == var, ]
    
  })

variables <-
  bind_rows(variables, new_rows)


# Join amenities to DA ----------------------------------------------------
DA <-
  left_join(DA, DA_amenities$DA, by = "ID") |>
  relocate(geometry, .after = last_col())

# Join amenities to CT ----------------------------------------------------
CT <-
  left_join(CT, DA_amenities$CT, by = "ID") |>
  relocate(geometry, .after = last_col())

# Join amenities to borough -----------------------------------------------
borough <-
  left_join(borough, DA_amenities$borough, by = "ID") |>
  relocate(geometry, .after = last_col())

# Join amenities to centraide ---------------------------------------------
centraide <-
  left_join(centraide, DA_amenities$centraide, by = "ID") |>
  relocate(geometry, .after = last_col())


# Clean up ----------------------------------------------------------------

rm(DA_amenities, DA_amenities_q3, DA_amenities_q5, DA_street_centroid, daycares, 
   daycares_spot, food_distribution, municipal_parks, municipal_parks_sqkm, 
   new_rows, old_plan, point_amenities, schools, sm_industry, var_list,
   susmontreal_bbox)


