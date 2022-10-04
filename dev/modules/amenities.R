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
#   st_filter(filter(CSD, !str_starts(ID, "2466023") & name != "Longueuil"))
# 
# municipal_parks <-
#   rbind(municipal_parks_island_longueuil, select(municipal_parks_osm)) |>
#   st_cast("MULTIPOLYGON") |>
#   st_cast("POLYGON") |>
#   transmute(id = row_number())
# 
# qsave(municipal_parks, file = "dev/data/amenity_access/municipal_parks.qs")


# Load and clean data -----------------------------------------------------

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
                          "Dental Laboratories",
                          "Civic, Social, and Fraternal Associations",
                          "Educational, Religious, and Charitable Trusts",
                          "Child Day Care Services")) |> 
  select(-sic)


# Schools
schools <- 
  list(public = read_sf("dev/data/amenity_access/PPS_Public_Ecole.shp"),
       private = read_sf("dev/data/amenity_access/PPS_Prive_Installation.shp") |> 
         rename(NOM_OFF_O = NOM_OFFCL)) |> 
  map(st_filter, DA) |> 
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

# The latter is created in dev/other/tt_matrix_DA.R
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


# For each point data, in which variable will they belong -----------------

# Food distributors
food_distribution_vars <- 
  c(unique(food_distribution$industry) |> 
      str_to_lower() |> 
      str_replace_all("\\s", "_")) |> 
  str_remove("\\_\\(.*") |> 
  str_remove_all(paste0("_and_fish_stores|_stores|_food|retail_|_products|",
                        "_and_vegetable_markets|_and_fish"))
names(food_distribution_vars) <- unique(food_distribution$industry)

food_distribution <- 
  food_distribution |> 
  mutate(vars = case_when(
    industry == "Grocery Stores" ~ list(c("grocery", "total")),
    industry == "Miscellaneous Food Stores" ~ list(c("miscellaneous", "total")),
    industry == "Retail Bakeries" ~ list(c("bakeries", "total")),
    industry == "Fruit and Vegetable Markets" ~ list(c("fruit", "total")),
    industry == "Meat and Fish (Seafood) Markets, Including Freezer Provisioners" ~ 
      list(c("meat", "total")),
    industry == "Dairy Products Stores" ~ list(c("dairy", "total")),
  ))

# Schools
schools <- 
  map_dfr(seq_len(nrow(schools)), function(r) {
    vars_fit <- 
      schools[r, ] |> 
      st_drop_geometry() |> 
      select(id, preschool, primary, secondary, vocational, adult) |> 
      pivot_longer(!id) |> 
      filter(value == 1) |> 
      pull(name)
    
    vars_fit_total <- paste(vars_fit, "total", sep = "_")
    
    public_private <- if (schools[r, ]$public) "public" else "private"
    
    vars_fit_public_private <- paste(vars_fit, public_private, sep = "_")
    total_public_pvriate <- paste0("total_", public_private)
    
    all_vars <- c(vars_fit_total, vars_fit_public_private, total_public_pvriate,
                  "total_total")
    
    schools[r, ] |> 
      mutate(vars = list(all_vars))
  })

# Health care

# TKTK


# Number of amenities per DA ----------------------------------------------

point_amenities <- 
  list(food_distribution = food_distribution, 
       schools = schools,
       health_care = health_care)

time_thresholds <- which(1:60 %% 5 == 0)

# Join all amenities to a DA ID, unnest vars (Even as it leads to duplicates IDs,
# there is only one combination of unique ID - vars) and drop geometry.
DA_amenities <- 
  furrr::future_map(point_amenities, function(df) {
    out <- st_join(df, select(DA, ID)) 
    if ("vars" %in% names(out)) out <- unnest(out, vars)
    st_drop_geometry(out)
  })

# Get the amount of amenities reachable per mode
progressr::with_progress({
  
  p <- 
    progressr::progressor(length(DA_amenities) *
                            length(tt_matrix_DA) *
                            length(tt_matrix_DA[[1]]) *
                            length(time_thresholds))
  DA_amenities <- 
    imap(DA_amenities, function(amenity, n_amenity) {
      imap(tt_matrix_DA, function(mode, n_mode) {
        imap(mode, function(timing, n_timing) {
          imap(set_names(time_thresholds), function(threshold, n_time_threshold) {
            p()
            
            if ("vars" %in% names(amenity)) {
              vars <- unique(amenity$vars)
              
              imap(set_names(vars), function(var, n_var) {
                filtered_am <- amenity[amenity$vars == var, ]

                filtered_am |>  
                  left_join(timing[timing$travel_time_p50 <= threshold, ], 
                            by = c("ID" = "to_id")) |> 
                  count(from_id, name = paste(n_amenity, n_var, str_to_lower(n_mode),
                                              n_timing, n_time_threshold,
                                              "count", sep = "_"))
              })
            } else {
              amenity |> 
                left_join(timing[timing$travel_time_p50 <= threshold, ], 
                          by = c("ID" = "to_id")) |> 
                count(from_id, name = paste(n_amenity, str_to_lower(n_mode),
                                            n_timing, n_time_threshold, "count", sep = "_"))
            }
          })
        })
      })
    })
})

DA_amenities <-
  map(DA_amenities, function(amenity) {
    map(amenity, function(mode) {
      map(mode, function(timing) {
        map(timing, function(threshold) {
          if (!is.data.frame(threshold)) {
            reduce(threshold, left_join, by = "from_id")
          } else threshold
        }) |> reduce(left_join, by = "from_id")
      }) |> reduce(left_join, by = "from_id")
    }) |> reduce(left_join, by = "from_id")
  })

sus_map <- function(x) {
  z <- 
  if (vec_depth(x) > 3) map(x, sus_map) else reduce(x, left_join, by = "from_id")
  
  if (!is.data.frame(z)) map(x, sus_map) else return(z)
}

# For parks, the amount of amenities reachable per mode in 15 minutes must
# be in sqkm
municipal_parks_sqkm <- st_join(municipal_parks, select(DA, ID)) |> 
  st_drop_geometry()
DA_amenities$municipal_parks_sqkm <-
  imap(tt_matrix_DA, function(mode, n_mode) {
    imap(mode, function(timing, n_timing) {
      imap(set_names(time_thresholds), function(threshold, n_time_threshold) {
        out <-
          municipal_parks_sqkm |> 
          left_join(timing[timing$travel_time_p50 <= threshold, ],
                    by = c("ID" = "to_id")) |>
          group_by(from_id) |>
          summarize(amenities = sum(sqkm),)
        names(out)[2] <- paste("municipal_parks", str_to_lower(n_mode),
                               n_timing, n_time_threshold, "sqkm", sep = "_")
        out
      }) |> reduce(left_join, by = "from_id")
    }) |> reduce(left_join, by = "from_id")
  }) |> reduce(left_join, by = "from_id")

# For daycares, the amount of amenities reachable per mode in 15 minutes must
# be per spots
daycares_spot <- st_join(daycares, select(DA, ID))
DA_amenities$daycares_spot <-
  imap(tt_matrix_DA, function(mode, n_mode) {
    imap(mode, function(timing, n_timing) {
      imap(set_names(time_thresholds), function(time_threshold, n_time_threshold) {
        out <- 
          timing[timing$travel_time_p50 <= time_threshold, ] |> 
          left_join(daycares_spot, by = c("to_id" = "ID")) |> 
          filter(!st_is_empty(geometry)) |> 
          group_by(from_id) |> 
          summarize(amenities = sum(spots_total))
        names(out)[2] <- paste("daycare_spots", str_to_lower(n_mode),
                                n_timing, n_time_threshold, "count", sep = "_")
        out
      }) |> reduce(left_join, by = "from_id")
    }) |> reduce(left_join, by = "from_id")
  }) |> reduce(left_join, by = "from_id")

# Bind all to one dataframe
DA_amenities <- 
  reduce(DA_amenities, left_join, by = "from_id") |> 
  rename_with(~paste0("amenities_", .x)) |> 
  rename(ID = amenities_from_id)


# Get DA amenities at other scales ----------------------------------------
# On average, an individual living in this scale can reach X amenity in a Y 
# minutes trip

# Add all DAs, change NAs for 0s
DA_amenities <- 
  DA_amenities |> 
  right_join(st_drop_geometry(select(DA, ID)), 
             by = "ID") |> 
  mutate(across(where(is.numeric), ~
                  ifelse(is.na(.x), 0, .x)))

DA_amenities <- 
  interpolate_scales(data = DA_amenities,
                     base_scale = "DA",
                     all_tables = all_tables,
                     weight_by = "population")


# Calculate breaks --------------------------------------------------------

DA_amenities <- calculate_breaks(DA_amenities)


# Assign to existing geographies ------------------------------------------

assign_tables(module_tables = DA_amenities)


# Add to variables table --------------------------------------------------

var_list <-
  DA_amenities$tables_list[[1]] |>
  select(-ID, -contains(c("q3", "q5"))) |>
  names()

# Get breaks_q3
breaks_q3_active <-
  imap_dfr(DA_amenities$tables_q3, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_active <-
  imap_dfr(DA_amenities$tables_q5, function(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, date = NA, rank = 0:5,
                                 .before = 1)})

interpolation_keys <- 
  map(set_names(names(DA_amenities$tables_list)), ~{
    if (.x == "DA") FALSE else "dissemination area"
  })

# Add in variables table
new_rows <-
  map_dfr(var_list, function(var) {
    
    # Transportation mode
    mode <- 
      case_when(str_detect(var, "_walk_") ~ "walk",
                str_detect(var, "_transit_") ~ "transit",
                str_detect(var, "_bicycle_") ~ "bicycle",
                str_detect(var, "_car_") ~ "car")
    
    # Amenity
    amenity <- 
      if (str_detect(var, "_food_distribution_")) {
        case_when(
          str_detect(var, "_grocery_") ~ "Grocery Stores",
          str_detect(var, "_miscellaneous_") ~ "Miscellaneous Food Stores",
          str_detect(var, "_bakeries_") ~ "Retail Bakeries",
          str_detect(var, "_fruit_") ~ "Fruit and Vegetable Markets",
          str_detect(var, "_meat_") ~ "Meat and Fish (Seafood) Markets",
          str_detect(var, "_dairy_") ~ "Dairy Products Stores",
          TRUE ~ "food distributors") |> 
          str_to_lower()
      } else if (str_detect(var, "_schools_")) {
        
        public_or_private <- 
          case_when(
            str_detect(var, "_public_") ~ "public ",
            str_detect(var, "_private_") ~ "private ",
            TRUE ~ "")
        
        school_level <- 
          case_when(
            str_detect(var, "_preschool_") ~ "preschools",
            str_detect(var, "_primary_") ~ "primary schools",
            str_detect(var, "_secondary_") ~ "secondary schools",
            str_detect(var, "_vocational_") ~ "vocational schools",
            str_detect(var, "_adult_") ~ "schools for adults",
            TRUE ~ "schools")
        
        paste0(public_or_private, school_level)
        
      } else {
        case_when(str_detect(var, "_daycare_spots_") ~ "daycare spots",
                  str_detect(var, "_health_care_") ~ "health care facilities",
                  str_detect(var, "_municipal_parks_") ~ 
                    "square kilometers of municipal parks")
      }
    
    # Short version of amenity
    amenity_short <- 
      if (str_detect(var, "_food_distribution_")) {
        case_when(
          str_detect(var, "_grocery_") ~ "Groceries",
          str_detect(var, "_miscellaneous_") ~ "Misc. Food",
          str_detect(var, "_bakeries_") ~ "Bakeries",
          str_detect(var, "_fruit_") ~ "Fruits",
          str_detect(var, "_meat_") ~ "Meat",
          str_detect(var, "_dairy_") ~ "Dairy",
          TRUE ~ "Food")
      } else if (str_detect(var, "_schools_")) {
        
        public_or_private <- 
          case_when(
            str_detect(var, "_public_") ~ "Pub. ",
            str_detect(var, "_private_") ~ "Pri. ",
            TRUE ~ "")
        
        school_level <- 
          case_when(
            str_detect(var, "_preschool_") ~ "Preschools",
            str_detect(var, "_primary_") ~ "Primary",
            str_detect(var, "_secondary_") ~ "Secondary",
            str_detect(var, "_vocational_") ~ "Cocational",
            str_detect(var, "_adult_") ~ "Adults",
            TRUE ~ "Schools")
        
        paste0(public_or_private, school_level)
        
      } else {
        case_when(str_detect(var, "_daycare_spots_") ~ "Daycare",
                  str_detect(var, "_health_care_") ~ "Health",
                  str_detect(var, "_municipal_parks_") ~ 
                    "Parks")
      }

    # Explanation
    pre <- 
      case_when(str_ends(var, "_count") ~ "the count",
                str_ends(var, "_sqkm") ~ "the number")
    explanation_ <- 
      if (mode == "walk") {
        glue::glue("{pre} of {amenity} accessible in a 15 minutes {mode}")
      } else {
        glue::glue("{pre} of {amenity} accessible in 15 minutes by {mode}")
      }
    
    # Higher level categories
    amenity_higher_level <- 
      case_when(str_detect(var, "_food_distribution_") ~ "food distributors",
                str_detect(var, "_schools_") ~ "schools",
                str_detect(var, "_daycare_spots_") ~ "daycare spots",
                str_detect(var, "_health_care_") ~ "health care facilities",
                str_detect(var, "_municipal_parks_") ~ 
                  "square kilometers of municipal parks")
    
    # Differentation between categories, for multiple dropdowns
    group_diff <- 
      if (str_detect(var, "_food_distribution_")) {
        cat <- 
          case_when(
            str_detect(var, "_grocery_") ~ "Grocery stores",
            str_detect(var, "_miscellaneous_") ~ "Miscellaneous food stores",
            str_detect(var, "_bakeries_") ~ "Retail bakeries",
            str_detect(var, "_fruit_") ~ "Fruit and vegetable markets",
            str_detect(var, "_meat_") ~ "Meat and fish (seafood) markets",
            str_detect(var, "_dairy_") ~ "Dairy products stores",
            TRUE ~ "Total") 
        
        list("Mode of transport" = as.character(glue::glue("By {mode}")),
             "Industry" = cat)
        
      } else if (str_detect(var, "_schools_")) {
        
        public_or_private <- 
          case_when(
            str_detect(var, "_public_") ~ "Public",
            str_detect(var, "_private_") ~ "Private",
            TRUE ~ "Total")
        
        school_level <- 
          case_when(
            str_detect(var, "_preschool_") ~ "Preschools",
            str_detect(var, "_primary_") ~ "Primary",
            str_detect(var, "_secondary_") ~ "Secondary",
            str_detect(var, "_vocational_") ~ "Vocational",
            str_detect(var, "_adult_") ~ "Adults",
            TRUE ~ "Total")
        
        list("Mode of transport" = as.character(glue::glue("By {mode}")),
             "Educational establishment category" = school_level,
             "Public/Private" = public_or_private)
        
      } else {
        list("Mode of transport" = as.character(glue::glue("By {mode}")))
      }
    
    # ADDED ROW
    out <-
      add_variables(variables,
                    var_code = var,
                    var_title = glue::glue("Accessibility to {amenity} by {mode}"),
                    var_short = glue::glue("{amenity_short} ({mode})"),
                    explanation = explanation_,
                    category = NA,
                    theme = "Accessibility to amenities",
                    private = TRUE,
                    dates = NA,
                    scales = names(DA_amenities$tables_list),
                    breaks_q3 = select(breaks_q3_active,
                                       scale, date, rank, 
                                       var = all_of(var)),
                    breaks_q5 = select(breaks_q5_active,
                                       scale, date, rank, 
                                       var = all_of(var)),
                    source = "Centraide",
                    interpolated = interpolation_keys,
                    grouping = as.character(glue::glue("Accessibility to {amenity_higher_level}")),
                    group_diff = group_diff)
    
    out[out$var_code == var, ]
    
  })

variables <-
  bind_rows(variables, new_rows)


# Clean up ----------------------------------------------------------------

rm(DA_amenities, DA_street_centroid, daycares, health_care, food_distribution_vars,
   daycares_spot, food_distribution, municipal_parks, municipal_parks_sqkm, 
   new_rows, old_plan, point_amenities, schools, sm_industry, var_list)


