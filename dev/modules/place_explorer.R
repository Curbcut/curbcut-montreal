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



# Title text highlights ---------------------------------------------------

# Value, Percentile (group + region.)
# 3 groups: Island, North shore (including Laval), South shore.

# Air quality for this location is X. It ranks at the X percentile on the
# island/north shore/south shore, and X for the wider region of Montreal.

# Groupings
groups <- 
  bind_rows(
  tibble(CSDUID = 
  c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
    "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
    "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
    "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
    "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
    "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
    "2466142", "2466072"),
  island = TRUE)) |> 
  (\(x) rbind(x, tibble(CSDUID = borough$CSDUID[!borough$CSDUID %in% 
                                                  pull(x, CSDUID)],
                     island = FALSE)))()
   
   
CSDUID_groups <- map(set_names(c("borough", "CT", "DA")), ~{
  get(.x) |> 
    st_drop_geometry() |> 
    select(ID, CSDUID) |> 
    left_join(groups, by = "CSDUID")
})

# Max census year
census_max <- 
  variables |> 
  filter(source == "census") |> 
  pull(dates) |> 
  unlist() |> 
  unique() |> 
  max() |> 
  as.numeric()

# DA 1000m buffers with ID and CSDUID
# DA_buffer <- 
#   DA |> 
#   select(ID, CSDUID) |> 
#   rowwise() |> 
#   mutate(buffer = st_buffer(st_centroid(geometry), 1000)) |> 
#   ungroup() |> 
#   st_set_geometry("buffer") |> 
#   select(-geometry)

# qsave(DA_buffer, file = "DA_buffer.qs")
DA_buffer <- qread("DA_buffer.qs")

# Function taking a df of 3 columns: ID, CSDUID and the column to calculate on.
percentile_calc <- function(x) {
  
  island_ranks <- 
    x |> 
    left_join(groups, by = "CSDUID") |> 
    filter(island) |> 
    (\(x) mutate(x, island_percentile = percent_rank(x[[3]])) |> 
       mutate(x, island_rank = rank(x[[3]])))()
  
  region_ranks <- 
    x |> 
    left_join(groups, by = "CSDUID") |> 
    (\(x) mutate(x, region_percentile = percent_rank(x[[3]])) |> 
       mutate(x, region_rank = rank(x[[3]])))() |> 
    rename(var = 3)
  
  left_join(region_ranks, island_ranks, by = c("ID", "CSDUID")) |> 
    select(ID, CSDUID, var, island_percentile, island_rank, region_percentile, 
           region_rank)
  
}

# Prepare list to store pre-computed title card indicators.
title_card_indicators <- list()

# Prepare tibble for easy indexing
title_card_index <- tibble(name = as.character(),
                           island_only = as.logical(),
                           date = as.numeric())

## Driving mode share - census --------------------------------------------
title_card_indicators <- 
  append(title_card_indicators, 
         list("transit_walk_cycle_share" =
                map(set_names(c("borough", "CT", "DA")), ~{
                  get(.x) |> 
                    st_drop_geometry() |> 
                    select(ID, CSDUID, paste0("trans_walk_or_bike_pct_", census_max),
                           paste0("trans_transit_pct_", census_max)) |> 
                    (\(x) mutate(x, transit_walk_cycle = x[[3]] + x[[4]]))() |> 
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "transit_walk_cycle_share",
          island_only = FALSE,
          date = census_max)

# Number of transit stops - Mtl open data ---------------------------------

commuters_stations <- 
  read_sf("dev/data/place_explorer/mtl_commuterstn.shp") |> 
  select(-everything()) |> 
  st_transform(4326) |> 
  mutate(station = TRUE)

metro_stations <- 
  read_sf("dev/data/place_explorer/mtl_metrostn.shp") |> 
  select(-everything()) |> 
  st_transform(4326) |> 
  mutate(station = TRUE)

bus_stations <- 
  read_sf("dev/data/place_explorer/region_bus.shp") |> 
  select(line) |> 
  st_transform(4326) |> 
  mutate(station = TRUE)

bixi_stations <- 
  read_csv("dev/data/place_explorer/bixi_stations.csv") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  select(-name) |> 
  mutate(station = TRUE)

title_card_indicators <- 
  append(title_card_indicators, 
         list("number_transit_stations" =
                map(set_names(c("borough", "CT", "DA")), ~{
                  
                  data <- if (.x %in% c("borough", "CT")) get(.x) else DA_buffer
                  
                  # Bixi stations
                  bixi_stations_ <- 
                    data |>
                    select(ID, CSDUID) |> 
                    st_join(bixi_stations) |> 
                    st_drop_geometry() |> 
                    filter(station) |> 
                    group_by(ID, CSDUID) |> 
                    summarize(bixi_stations = n(), .groups = "drop")
                  
                  # Bus lines
                  bus_lines_ <- 
                    data |>
                    select(ID, CSDUID) |> 
                    st_join(bus_stations) |> 
                    st_drop_geometry() |> 
                    filter(station) |> 
                    group_by(ID, CSDUID) |> 
                    distinct(line) |> 
                    summarize(bus_lines = n(), .groups = "drop")
                  
                  # Bus stops
                  bus_stops_ <- 
                    data |>
                    select(ID, CSDUID) |> 
                    st_join(bus_stations) |> 
                    st_drop_geometry() |> 
                    filter(station) |> 
                    group_by(ID, CSDUID) |> 
                    summarize(bus_stops = n(), .groups = "drop")
                  
                  # Metro station
                  metro_stations_ <- 
                    data |>
                    select(ID, CSDUID) |> 
                    st_join(metro_stations) |> 
                    st_drop_geometry() |> 
                    filter(station) |> 
                    group_by(ID, CSDUID) |> 
                    summarize(metro_stations = n(), .groups = "drop")
                  
                  # Commuter rail stations
                  commuters_stations_ <- 
                    data |>
                    select(ID, CSDUID) |> 
                    st_join(commuters_stations) |> 
                    st_drop_geometry() |> 
                    filter(station) |> 
                    group_by(ID, CSDUID) |> 
                    summarize(commuters_stations = n(), .groups = "drop")
                  
                  get(.x) |> 
                    select(ID, CSDUID) |> 
                    left_join(bixi_stations_, by = c("ID", "CSDUID")) |> 
                    left_join(bus_lines_, by = c("ID", "CSDUID")) |> 
                    left_join(bus_stops_, by = c("ID", "CSDUID")) |> 
                    left_join(metro_stations_, by = c("ID", "CSDUID")) |> 
                    left_join(commuters_stations_, by = c("ID", "CSDUID"))
                  
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "transit_bixi",
          island_only = FALSE,
          date = c(2020, 2021))


## Number of crashes - Mtl data portal ------------------------------------
last_crash_data_year <- 
names(borough) |>
  str_subset("crash_total_per1k") |> 
  str_extract("\\d{4}") |> 
  as.numeric() |> 
  max()

title_card_indicators <- 
  append(title_card_indicators, 
         list("total_crash_per1k" =
                map(set_names(c("borough", "CT", "DA")), ~{
                  get(.x) |> 
                    st_drop_geometry() |> 
                    select(ID, CSDUID, paste0("crash_total_per1k_", 
                                              last_crash_data_year)) |> 
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "total_crash_per1k",
          # explanation = "The total number of crash per square km",
          island_only = TRUE,
          date = last_crash_data_year)

## Air quality - PM2.5 - CANUE --------------------------------------------

## Air quality - NO2 - CANUE ----------------------------------------------

no2 <- read_csv("dev/data/place_explorer/no2lur_a_16.csv") |> 
  st_as_sf(coords = c("longitude", "latitude"),crs = 4326) |> 
  select(-postalcode16, -province) |> 
  st_filter(borough) |> 
  rename(NO2 = 1)

# pm25 <- read_csv("dev/data/place_explorer/pm25dalc_a_18.csv") |> 
#   st_as_sf(coords = c("longitude", "latitude"),crs = 4326) |> 
#   select(-postalcode18, -province) |> 
#   st_filter(borough) |> 
#   rename(PM25 = 1)

title_card_indicators <- 
  append(title_card_indicators, 
         list("air_quality_no2" =
                map(set_names(c("borough", "CT", "DA")), ~{
                  get(.x) |> 
                    select(ID, CSDUID) |> 
                    st_join(no2) |> 
                    st_drop_geometry() |> 
                    filter(NO2 != -9999) |> 
                    group_by(ID, CSDUID) |> 
                    summarize(NO2 = mean(NO2), .groups = "drop") |> 
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "air_quality_no2",
          # explanation = "The air quality measurement (NO2)",
          island_only = FALSE,
          date = 2016)

## Percentage of Single Family Homes - Census -----------------------------

title_card_indicators <-
  append(title_card_indicators,
         list("single_detached" =
                map(set_names(c("borough", "CT", "DA")), ~{
                  get(.x) |>
                    st_drop_geometry() |>
                    select(ID, CSDUID, paste0("housing_single_detached_pct_", census_max)) |>
                    percentile_calc()
                })
         ))

title_card_index <-
  title_card_index |>
  add_row(name = "single_detached",
          island_only = FALSE,
          date = census_max)

## Amount of greenspace in spatial unit - Mtl data portal -----------------


## Distance to nearest green space - Mtl data portal ----------------------


## Green space, NDVI - CANUE ----------------------------------------------
ndvi <- read_csv("dev/data/place_explorer/grlan_amn_19.csv") |> 
  st_as_sf(coords = c("longitude", "latitude"),crs = 4326) |> 
  select(-postalcode19, -province) |> 
  st_filter(borough) |> 
  # I believe the first value is Annual value, Mean of Means 100m.
  select(NDVI = 1)

title_card_indicators <- 
  append(title_card_indicators, 
         list("green_space_ndvi" =
                map(set_names(c("borough", "CT", "DA")), ~{
                  get(.x) |> 
                    select(ID, CSDUID) |> 
                    st_join(ndvi) |> 
                    st_drop_geometry() |> 
                    filter(NDVI != -9999) |> 
                    group_by(ID, CSDUID) |> 
                    summarize(NDVI = mean(NDVI), .groups = "drop") |> 
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "green_space_ndvi",
          island_only = FALSE,
          date = 2019)

## Active Living potential - CanALE ---------------------------------------
title_card_indicators <-
  append(title_card_indicators,
         list("canale_index" =
                map(set_names(c("borough", "CT", "DA")), ~{
                  get(.x) |>
                    st_drop_geometry() |>
                    select(ID, CSDUID, paste0("canale_ind_", census_max)) |>
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "canale_index",
          island_only = FALSE,
          date = census_max)



# Get percentile of variables, to order in place ex -----------------------

# Percentile retrieval
# Which variables should have a percentile attached?
basic_percentile_retrieval <- 
  variables |> 
  filter(source == "census" |
           str_starts(var_code, "climate") |
           str_starts(var_code, "canale"))

pe_var_hierarchy <- 
  map(set_names(c("borough", "CT", "DA")), function(scale) {
    map(set_names(basic_percentile_retrieval$var_code), function(variable_code) {
      
      var_row <- variables[variables$var_code == variable_code, ]
      var <- var_row$var_code
      max_date <- unlist(var_row$dates)[length(unlist(var_row$dates))]
      
      if (!is.na(max_date)) {
        var <- paste(var, max_date, sep = "_")
      }
      
      out <- 
        get(scale) |> 
        st_drop_geometry() |> 
        select(ID, var = all_of(var)) |> 
        mutate(var_percentile = percent_rank(var))
      
      names(out) <- c("ID", var, paste0(var, "_percentile"))
      
      out
      
    }) |> reduce(left_join, by = "ID")
  })

# Add gentrification from the minimum date to its maximum.
gentrification_min_max <- 
  borough |> 
  st_drop_geometry() |> 
  select(starts_with("gentrification")) |> 
  names() |> 
  str_extract("\\d{4}$") |> 
  unique() |> 
  (\(x) list(min = min(x), max = max(x)))()

pe_var_hierarchy <-
  map(set_names(names(pe_var_hierarchy)), ~{
    
    id_gentrification_ind <- 
      get(.x) |> 
      st_drop_geometry() |> 
      select(ID, starts_with("gentrification") & 
               ends_with(c(gentrification_min_max$min, 
                           gentrification_min_max$max)),
             -contains(c("q3", "q5"))) |> 
      (\(x) mutate(x, gentrification_ind = (x[[3]] - x[[2]]) /
                     x[[2]]))() |> 
      select(ID, gentrification_ind) |> 
      mutate(gentrification_ind_percentile = 
               percent_rank(gentrification_ind))
    
    left_join(pe_var_hierarchy[[.x]], id_gentrification_ind, by = "ID")
    
  })

# Retrieve access average values
min_access_var_code <- 
  variables |> 
  filter(str_starts(var_code, "access")) |>
  mutate(var_code = case_when(str_starts(var_code, "access_jobs") ~ 
                                str_extract(var_code, "access_jobs_[^_]*"),
                              TRUE ~ str_extract(var_code, "access_[^_]*"))) |> 
  pull(var_code) |> 
  unique()

pe_var_hierarchy[["CT"]] <- 
  left_join(pe_var_hierarchy[["CT"]], 
            map(set_names(min_access_var_code), ~{
              out <- 
                CT |> 
                st_drop_geometry() |> 
                select(ID, starts_with(.x)) |> 
                pivot_longer(-ID) |> 
                group_by(ID) |> 
                summarize(var = mean(value)) |> 
                mutate(percentile = 
                         percent_rank(var))
              
              names(out) <- c("ID", .x, paste0(.x, "_percentile"))
              
              out
            }) |> reduce(left_join, by = "ID"),
            by = "ID")


# Put hierarchy in place --------------------------------------------------

# For each geometry, each ID will have to order both THEMES together to know
# which theme to show up first, + intra-theme which VARIBLES to show first
pe_theme_order <-
  map(set_names(names(pe_var_hierarchy)), ~{
    
    place_ex_variables <- 
      rbind(filter(variables, !str_starts(var_code, "access")),
            variables |> 
              filter(str_starts(var_code, "access")) |>
              mutate(var_code = case_when(str_starts(var_code, "access_jobs") ~ 
                                            str_extract(var_code, "access_jobs_[^_]*"),
                                          TRUE ~ str_extract(var_code, "access_[^_]*")))
      )
    
    pe_var_hierarchy[[.x]] |> 
      pivot_longer(-ID) |> 
      filter(str_ends(name, "percentile")) |> 
      transmute(ID, 
                var_code = str_remove(name, "_percentile"), 
                percentile = value) |> 
      mutate(max_or_min = abs(0.5 - percentile)) |> 
      left_join(select(place_ex_variables, var_code, theme), 
                by = c("var_code")) |> 
      group_by(ID, theme) |> 
      summarize(theme_order = mean(max_or_min), .groups = "drop") |> 
      group_by(ID) |> 
      arrange(-theme_order) |> 
      mutate(theme_order = row_number()) |> 
      ungroup()
  })

pe_variable_order <- 
  map(set_names(names(pe_var_hierarchy)), ~{
    
    place_ex_variables <- 
      rbind(filter(variables, !str_starts(var_code, "access")),
            variables |> 
              filter(str_starts(var_code, "access")) |>
              mutate(var_code = case_when(str_starts(var_code, "access_jobs") ~ 
                                            str_extract(var_code, "access_jobs_[^_]*"),
                                          TRUE ~ str_extract(var_code, "access_[^_]*")))
      )
    
    pe_var_hierarchy[[.x]] |> 
      pivot_longer(-ID) |> 
      filter(str_ends(name, "percentile")) |> 
      transmute(ID, 
                var_code = str_remove(name, "_percentile"), 
                percentile = value) |> 
      mutate(max_or_min = abs(0.5 - percentile)) |> 
      left_join(select(place_ex_variables, var_code, theme), 
                by = c("var_code")) |> 
      group_by(ID, theme) |> 
      arrange(-max_or_min) |> 
      mutate(variable_order = row_number()) |> 
      ungroup() |> 
      select(ID, theme, var_code, variable_order, theme)
  })

# Cleanup -----------------------------------------------------------------

rm(basic_percentile_retrieval, gentrification_min_max, min_access_var_code,
   bixi_stations, census_max, groups, last_crash_data_year, 
   ndvi, no2, percentile_calc, transit_stations)
