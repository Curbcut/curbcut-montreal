#### Place explorer data setup #################################################

# Import postal codes -----------------------------------------------------

postal_codes <- 
  read_csv("dev/data/ZipCodeFiles/CanadianPostalCodes202103.csv")

postal_codes <- 
  postal_codes |>  
  filter(PROVINCE_ABBR == "QC") |>
  select(-PROVINCE_ABBR, -TIME_ZONE) |> 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |> 
  setNames(c("postal_code", "city", "geometry")) |> 
  st_filter(master_polygon) |> 
  as_tibble() |> 
  st_as_sf() |> 
  mutate(postal_code = str_remove(str_to_lower(postal_code), "\\s")) |> 
  st_join(select(DA, DAUID), left = FALSE)

# Title text highlights ---------------------------------------------------

# Max census year
census_max <- 
  variables |> 
  filter(source == "Canadian census") |> 
  pull(dates) |> 
  unlist() |> 
  unique() |> 
  max() |> 
  as.numeric()

# Function taking a df of 3 columns: ID, geo_ID and the column to calculate on.
percentile_calc <- function(x) {
    x |> 
    rename(var = 3) |> 
    mutate(percentile = percent_rank(var),
           rank = rank(var))
}

# Prepare list to store pre-computed title card indicators.
title_card_indicators <- list()

# Prepare tibble for easy indexing
title_card_index <- tibble(name = as.character(),
                           title = as.character(),
                           island_only = as.logical(),
                           date = as.numeric(),
                           percent = as.logical(),
                           high_is_good = as.logical(),
                           val_digits = 0,
                           text = as.character(),
                           link_module = as.character(),
                           link_var_left = as.character(),
                           link_outside = as.character())

all_tables_DA_max <- map(all_tables, ~.x[seq_len(which(.x == "DA"))])

# Map over all tables function
map_all_tables <- function(all_tables_DA_max, fun) {
  imap(all_tables_DA_max, function(scales, geo) {
    map(set_names(scales), function(scale) {
      geo_scale <- paste(geo, scale, sep = "_")
      df <- get(geo_scale)
      fun(df)
    })
  })
}


## Driving mode share - census --------------------------------------------
title_card_indicators <- 
  append(title_card_indicators, 
         list("transit_walk_cycle_share" =
                map_all_tables(all_tables_DA_max, fun = \(x) {
                  st_drop_geometry(x) |> 
                    select(ID, geo_ID, any_of(c(
                      paste0("trans_walk_or_bike_pct_", census_max),
                      paste0("trans_transit_pct_", census_max)))) |> 
                    (\(x) mutate(x, transit_walk_cycle = x[[3]] + x[[4]]))() |> 
                    select(ID, geo_ID, transit_walk_cycle) |> 
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "transit_walk_cycle_share",
          title = "Sustainable transport",
          island_only = FALSE,
          date = census_max,
          percent = TRUE,
          high_is_good = TRUE, 
          val_digits = 0,
          text = paste0("{z$pretty_data_var} of residents ",
                        "use public transit, walk or ",
                        "bicycle to get to work. {z$data_rank}. ",
                        "(Data from {z$data_date})"))

# Number of transit stops - Mtl open data ---------------------------------

# commuters_stations <- 
#   read_sf("dev/data/place_explorer/mtl_commuterstn.shp") |> 
#   select(-everything()) |> 
#   st_transform(4326) |> 
#   mutate(station = TRUE)
# 
# metro_stations <- 
#   read_sf("dev/data/place_explorer/mtl_metrostn.shp") |> 
#   select(-everything()) |> 
#   st_transform(4326) |> 
#   mutate(station = TRUE)
# 
# bus_stations <- 
#   read_sf("dev/data/place_explorer/region_bus.shp") |> 
#   select(line) |> 
#   st_transform(4326) |> 
#   mutate(station = TRUE)
# 
# bixi_stations <- 
#   read_csv("dev/data/place_explorer/bixi_stations.csv") |> 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
#   select(-name) |> 
#   mutate(station = TRUE)
# 
# title_card_indicators <- 
#   append(title_card_indicators, 
#          list("number_transit_stations" =
#                 map(set_names(c("borough", "CT", "DA")), ~{
#                   
#                   data <- if (.x %in% c("borough", "CT")) get(.x) else DA_buffer
#                   
#                   # Bixi stations
#                   bixi_stations_ <- 
#                     data |>
#                     select(ID, CSDUID) |> 
#                     st_join(bixi_stations) |> 
#                     st_drop_geometry() |> 
#                     filter(station) |> 
#                     group_by(ID, CSDUID) |> 
#                     summarize(bixi_stations = n(), .groups = "drop")
#                   
#                   # Bus lines
#                   bus_lines_ <- 
#                     data |>
#                     select(ID, CSDUID) |> 
#                     st_join(bus_stations) |> 
#                     st_drop_geometry() |> 
#                     filter(station) |> 
#                     group_by(ID, CSDUID) |> 
#                     distinct(line) |> 
#                     summarize(bus_lines = n(), .groups = "drop")
#                   
#                   # Bus stops
#                   bus_stops_ <- 
#                     data |>
#                     select(ID, CSDUID) |> 
#                     st_join(bus_stations) |> 
#                     st_drop_geometry() |> 
#                     filter(station) |> 
#                     group_by(ID, CSDUID) |> 
#                     summarize(bus_stops = n(), .groups = "drop")
#                   
#                   # Metro station
#                   metro_stations_ <- 
#                     data |>
#                     select(ID, CSDUID) |> 
#                     st_join(metro_stations) |> 
#                     st_drop_geometry() |> 
#                     filter(station) |> 
#                     group_by(ID, CSDUID) |> 
#                     summarize(metro_stations = n(), .groups = "drop")
#                   
#                   # Commuter rail stations
#                   commuters_stations_ <- 
#                     data |>
#                     select(ID, CSDUID) |> 
#                     st_join(commuters_stations) |> 
#                     st_drop_geometry() |> 
#                     filter(station) |> 
#                     group_by(ID, CSDUID) |> 
#                     summarize(commuters_stations = n(), .groups = "drop")
#                   
#                   get(.x) |> 
#                     select(ID, CSDUID) |> 
#                     left_join(bixi_stations_, by = c("ID", "CSDUID")) |> 
#                     left_join(bus_lines_, by = c("ID", "CSDUID")) |> 
#                     left_join(bus_stops_, by = c("ID", "CSDUID")) |> 
#                     left_join(metro_stations_, by = c("ID", "CSDUID")) |> 
#                     left_join(commuters_stations_, by = c("ID", "CSDUID"))
#                   
#                 })
#          ))
# 
# title_card_index <- 
#   title_card_index |> 
#   add_row(name = "transit_bixi",
#           island_only = FALSE,
#           date = c(2020, 2021),
#           percent = TRUE,
#           high_is_good = TRUE, 
#           val_digits = 0)


## Number of crashes - Mtl data portal ------------------------------------
# last_crash_data_year <- 
#   names(borough) |>
#   str_subset("crash_total_per1k") |> 
#   str_extract("\\d{4}") |> 
#   as.numeric() |> 
#   max()
# 
# title_card_indicators <- 
#   append(title_card_indicators, 
#          list("total_crash_per1k" =
#                 map(set_names(c("borough", "CT", "DA")), ~{
#                   get(.x) |> 
#                     st_drop_geometry() |> 
#                     select(ID, CSDUID, paste0("crash_total_per1k_", 
#                                               last_crash_data_year)) |> 
#                     percentile_calc()
#                 })
#          ))
# 
# title_card_index <- 
#   title_card_index |> 
#   add_row(name = "total_crash_per1k",
#           title = "Road collisions",
#           island_only = TRUE,
#           date = last_crash_data_year,
#           percent = FALSE,
#           high_is_good = FALSE, 
#           val_digits = 0,
#           text = paste0("There were {z$pretty_data_var} total crashes ",
#                         "per 1,000 residents in {z$data_date}. ", 
#                         "{z$data_rank}."),
#           link_module = "crash",
#           link_var_left = "crash_total_per1k")

## Air quality - PM2.5 - CANUE --------------------------------------------

## Air quality - NO2 - CANUE ----------------------------------------------

no2 <- read_csv("dev/data/place_explorer/no2lur_a_16.csv") |> 
  st_as_sf(coords = c("longitude", "latitude"),crs = 4326) |> 
  select(-postalcode16, -province) |> 
  st_filter(master_polygon) |> 
  rename(NO2 = 1) |> 
  st_transform(st_crs(CMA_CSD))

# pm25 <- read_csv("dev/data/place_explorer/pm25dalc_a_18.csv") |> 
#   st_as_sf(coords = c("longitude", "latitude"),crs = 4326) |> 
#   select(-postalcode18, -province) |> 
#   st_filter(borough) |> 
#   rename(PM25 = 1)

title_card_indicators <- 
  append(title_card_indicators, 
         list("air_quality_no2" =
                map_all_tables(all_tables_DA_max, fun = \(x) {
                  x |> 
                    select(ID, geo_ID) |> 
                    st_join(no2) |> 
                    st_drop_geometry() |> 
                    filter(NO2 != -9999) |> 
                    group_by(ID, geo_ID) |> 
                    summarize(NO2 = mean(NO2), .groups = "drop") |> 
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "air_quality_no2",
          title = "Air pollution",
          island_only = FALSE,
          date = 2016,
          percent = FALSE,
          high_is_good = FALSE, 
          val_digits = 1,
          text = paste0("{z$data_rank} in terms of level of NO2 ",
                        "pollution. {higher_than_threshold}(NO2 = ",
                        "{z$pretty_data_var}, data from {z$data_date})"),
          link_outside = 
            "https://www.canuedata.ca/tmp/CANUE_METADATA_NO2LUR_A_YY.pdf")

## Percentage of Single Family Homes - Census -----------------------------

title_card_indicators <-
  append(title_card_indicators,
         list("single_detached" =
                map_all_tables(all_tables_DA_max, fun = \(x) {
                  x |>
                    st_drop_geometry() |>
                    select(ID, geo_ID, paste0("housing_single_detached_pct_", 
                                              census_max)) |>
                    percentile_calc()
                })
         ))

title_card_index <-
  title_card_index |>
  add_row(name = "single_detached",
          title = "Housing",
          island_only = FALSE,
          date = census_max,
          percent = TRUE,
          high_is_good = FALSE, 
          val_digits = 0,
          text = paste0("{z$pretty_data_var} of occupied dwellings are ",
                        "single-detached houses. {z$data_rank}. ",
                        "(Data from {z$data_date})"),
          link_module = "housing",
          link_var_left = "housing_single_detached_pct")

## Amount of greenspace in spatial unit - Mtl data portal -----------------


## Distance to nearest green space - Mtl data portal ----------------------


## Green space, NDVI - CANUE ----------------------------------------------
ndvi <- read_csv("dev/data/place_explorer/grlan_amn_19.csv") |> 
  st_as_sf(coords = c("longitude", "latitude"),crs = 4326) |> 
  select(-postalcode19, -province) |> 
  st_filter(master_polygon) |> 
  # I believe the first value is Annual value, Mean of Means 100m.
  select(NDVI = 1) |> 
  st_transform(st_crs(CMA_CSD))

title_card_indicators <- 
  append(title_card_indicators, 
         list("green_space_ndvi" =
                map_all_tables(all_tables_DA_max, fun = \(x) {
                  x |> 
                    select(ID, geo_ID) |> 
                    st_join(ndvi) |> 
                    st_drop_geometry() |> 
                    filter(NDVI != -9999) |> 
                    group_by(ID, geo_ID) |> 
                    summarize(NDVI = mean(NDVI), .groups = "drop") |> 
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "green_space_ndvi",
          title = "Green space",
          island_only = FALSE,
          date = 2019,
          percent = TRUE,
          high_is_good = TRUE, 
          val_digits = 0,
          text = paste0("{z$data_rank} in terms of green space. (",
                        "<a href='", 
                        "https://www.canuedata.ca/tmp/",
                        "CANUE_METADATA_GRAVH_AMN_YY.pdf", 
                        "' ","target='_blank'>", 
                        "NDVI", "</a> = {z$pretty_data_var}, ",
                        "data from {z$data_date})"),
          link_module = "green_space",
          link_var_left = "green_space_total_sqkm")

## Active Living potential - CanALE ---------------------------------------
title_card_indicators <-
  append(title_card_indicators,
         list("canale_index" =
                map_all_tables(all_tables_DA_max, fun = \(x) {
                  x |>
                    st_drop_geometry() |>
                    select(ID, geo_ID, paste0("canale_ind_", census_max)) |>
                    percentile_calc()
                })
         ))

title_card_index <- 
  title_card_index |> 
  add_row(name = "canale_index",
          title = "Active living",
          island_only = FALSE,
          date = census_max,
          percent = FALSE,
          high_is_good = TRUE, 
          val_digits = 0,
          text = paste0("{z$data_rank} in terms of active living. ",
                        "(Data from {z$data_date})"),
          link_module = "canale",
          link_var_left = NULL)



# Bring title cards in smaller lists of dfs -------------------------------

out_primero <- title_card_indicators
out_final <- map(set_names(names(out_primero)), ~NULL)

for (z in names(out_primero)) {
  out <- out_primero[[z]]
  for (a in seq_len(length(out))) {
    geo <- names(out)[[a]]
    for (b in seq_len(length(out[[geo]]))) {
      scale <- names(out[[geo]])[[b]]
      df_name <- paste(geo, scale, sep = "_")

      out_final[[z]][[df_name]] <- out_primero[[z]][[geo]][[scale]]
    }
  }
}

title_card_indicators <- out_final
rm(out_primero, out_final, out, geo, scale, df_name)

# Get percentile of variables, to order in place ex -----------------------

# Percentile retrieval
# Which variables should have a percentile attached?
basic_percentile_retrieval <- 
  variables |> 
  filter(source == "Canadian census" |
           str_starts(var_code, "climate")) |> 
  filter(var_code != "climate_flood_ind")

# Map over all tables function
map_all_tables <- function(all_tables_DA_max, fun) {
  imap(all_tables_DA_max, function(scales, geo) {
    map(set_names(scales), function(scale) {
      geo_scale <- paste(geo, scale, sep = "_")
      df <- get(geo_scale)
      fun(df)
    })
  })
}

all_tables_to_get <- 
  imap(all_tables_DA_max, function(scales, geo) {
    map_chr(set_names(scales), function(scale) {
      paste(geo, scale, sep = "_")
    })
  }) |> unlist() |> unname()

pe_var_hierarchy <- 
  map(set_names(all_tables_to_get), \(df) {
    map(set_names(basic_percentile_retrieval$var_code), \(variable_code) {
      
      var_row <- variables[variables$var_code == variable_code, ]
      max_date <- unlist(var_row$dates)[length(unlist(var_row$dates))]
      
      if (!is.na(max_date)) {
        var <- paste(variable_code, max_date, sep = "_")
      } else var <- variable_code
      
      get(df) |> 
        st_drop_geometry() |> 
        select(ID, geo_ID, all_of(var)) |> 
        percentile_calc()
      
    })
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

pe_var_hierarchy[["CMA_CT"]] <-
  append(pe_var_hierarchy[["CMA_CT"]],
            map(set_names(min_access_var_code), function(access_code) {
                CMA_CT |>
                st_drop_geometry() |>
                select(ID, geo_ID, starts_with(access_code)) |>
                pivot_longer(-c(ID, geo_ID)) |>
                group_by(ID, geo_ID) |>
                summarize(value = mean(value), .groups = "drop") |>
                ungroup() |> 
                percentile_calc()
            })
  )


# Put hierarchy in place --------------------------------------------------

# For each geometry, each ID will have to order both THEMES together to know
# which theme to show up first, + intra-theme which VARIBLES to show first
pe_theme_order <-
  map(set_names(names(pe_var_hierarchy)), \(x) {
    
    # Fix access, which is an average in this case
    place_ex_variables <- 
      rbind(filter(variables, !str_starts(var_code, "access")),
            variables |>
              filter(str_starts(var_code, "access")) |>
              mutate(var_code = case_when(
                str_starts(var_code, "access_jobs") ~
                  str_extract(var_code, "access_jobs_[^_]*"),
                TRUE ~ str_extract(var_code, "access_[^_]*")))
      )
    
    data <- pe_var_hierarchy[[x]]
    
    data <- 
      map(names(data), function(var_code) {
        names(data[[var_code]]) <- c("ID", "geo_ID", var_code, 
                                     paste0(var_code, "_percentile"),
                                     paste0(var_code, "_rank"))
        data[[var_code]]
      }) |> reduce(left_join, by = c("ID", "geo_ID"))
    
    data |>
      select(ID, contains("percentile")) |> 
      pivot_longer(-ID) |>
      transmute(ID, 
             var_code = str_remove(name, "_percentile"),
             percentile = value) |> 
      filter(!is.na(percentile)) |> 
      mutate(max_or_min = abs(0.5 - percentile)) |> 
      left_join(select(place_ex_variables, var_code, theme), 
                by = c("var_code")) |> 
      group_by(ID, theme) |> 
      summarize(standout_score = mean(max_or_min), .groups = "drop") |> 
      group_by(ID) |> 
      arrange(-standout_score) |> 
      mutate(theme_order = row_number()) |> 
      ungroup() |> 
      mutate(standout = case_when(standout_score > 0.4 ~ "Extreme outlier",
                                  standout_score > 0.3 ~ "Outlier",
                                  TRUE ~ "Typical"))
  })

pe_variable_order <- 
  map(set_names(names(pe_var_hierarchy)), \(x) {
    
    # Fix access, which is an average in this case
    place_ex_variables <- 
      rbind(filter(variables, !str_starts(var_code, "access")),
            variables |>
              filter(str_starts(var_code, "access")) |>
              mutate(var_code = case_when(
                str_starts(var_code, "access_jobs") ~
                  str_extract(var_code, "access_jobs_[^_]*"),
                TRUE ~ str_extract(var_code, "access_[^_]*")))
      )
    
    data <- pe_var_hierarchy[[x]]
    
    data <- 
      map(names(data), function(var_code) {
        names(data[[var_code]]) <- c("ID", "geo_ID", var_code, 
                                     paste0(var_code, "_percentile"),
                                     paste0(var_code, "_rank"))
        data[[var_code]]
      }) |> reduce(left_join, by = c("ID", "geo_ID"))
    
    data |>
      select(ID, contains("percentile")) |> 
      pivot_longer(-ID) |>
      transmute(ID, 
                var_code = str_remove(name, "_percentile"),
                percentile = value) |> 
      filter(!is.na(percentile)) |> 
      mutate(max_or_min = abs(0.5 - percentile)) |> 
      left_join(select(place_ex_variables, var_code, theme), 
                by = c("var_code")) |> 
      group_by(ID, theme) |> 
      arrange(-max_or_min) |> 
      mutate(variable_order = row_number()) |> 
      ungroup() |> 
      select(ID, theme, var_code, variable_order, theme)
  })


# Split tables by group ---------------------------------------------------

pe_variable_order <- lapply(pe_variable_order, \(x) split(x, x$ID)) 
pe_theme_order <- lapply(pe_theme_order, \(x) split(x, x$ID)) 


# Add to modules table ----------------------------------------------------

modules <- 
  modules |> 
  add_modules(id = "place_explorer",
              metadata = FALSE,
              dataset_info = "TKTK")

# Cleanup -----------------------------------------------------------------

rm(basic_percentile_retrieval, min_access_var_code,
   census_max, all_tables_DA_max, all_tables_to_get, 
   ndvi, no2, percentile_calc)
