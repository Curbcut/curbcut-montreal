#### Place explorer data setup #################################################

# This script relies on objects created in dev/census.R

island_CSDUID <- 
  c("2466007", "2466023_1",  "2466023_10", "2466023_11", "2466023_12", 
    "2466023_13", "2466023_14", "2466023_15", "2466023_16", "2466023_17", 
    "2466023_18", "2466023_19", "2466023_2", "2466023_3", "2466023_4", 
    "2466023_5",  "2466023_6", "2466023_7", "2466023_8", "2466023_9",
    "2466032", "2466047", "2466058", "2466062", "2466087", "2466092", 
    "2466097", "2466102", "2466107", "2466112", "2466117", "2466127", 
    "2466142", "2466072", "2466023")


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
  mutate(postal_code = str_remove(str_to_lower(postal_code), "\\s")) |> 
  st_join(select(DA, DAUID), left = FALSE)

# Title text highlights ---------------------------------------------------

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
  filter(source == "Canadian census") |> 
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
# 
# qsave(DA_buffer, file = "data/DA_1000m_buffer.qs")
# DA_buffer <- qread("data/DA_1000m_buffer.qs")

# Function taking a df of 3 columns: ID, CSDUID and the column to calculate on.
percentile_calc <- function(x) {
  
  var <- NULL
  
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

## Driving mode share - census --------------------------------------------
title_card_indicators <- 
  append(title_card_indicators, 
         list("transit_walk_cycle_share" =
                map(set_names(c("borough", "CT", "DA")), ~{
                  get(.x) |> 
                    st_drop_geometry() |> 
                    select(ID, CSDUID, paste0("trans_walk_or_bike_pct_", 
                                              census_max),
                           paste0("trans_transit_pct_", census_max)) |> 
                    (\(x) mutate(x, transit_walk_cycle = x[[3]] + x[[4]]))() |> 
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
                map(set_names(c("borough", "CT", "DA")), ~{
                  get(.x) |>
                    st_drop_geometry() |>
                    select(ID, CSDUID, paste0("housing_single_detached_pct_", 
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



# Get percentile of variables, to order in place ex -----------------------

# Percentile retrieval
# Which variables should have a percentile attached?
basic_percentile_retrieval <- 
  variables |> 
  filter(source == "Canadian census" |
           str_starts(var_code, "climate")) |> 
  filter(var_code != "climate_flood_ind")

pe_var_hierarchy <- 
  map(set_names(c("borough", "CT", "DA")), \(scale) {
    map(set_names(basic_percentile_retrieval$var_code), \(variable_code) {
      
      var_row <- variables[variables$var_code == variable_code, ]
      max_date <- unlist(var_row$dates)[length(unlist(var_row$dates))]
      
      if (!is.na(max_date)) {
        var <- paste(variable_code, max_date, sep = "_")
      } else var <- variable_code
      
      out <- 
        get(scale) |> 
        st_drop_geometry() |> 
        select(ID, CSDUID, all_of(var)) |> 
        percentile_calc()
      
      out
      
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

pe_var_hierarchy[["CT"]] <-
  append(pe_var_hierarchy[["CT"]],
            map(set_names(min_access_var_code), function(access_code) {
              
              out <-
                CT |>
                st_drop_geometry() |>
                select(ID, CSDUID, starts_with(access_code)) |>
                pivot_longer(-c(ID, CSDUID)) |>
                group_by(ID, CSDUID) |>
                summarize(value = mean(value), .groups = "drop") |>
                ungroup() |> 
                percentile_calc()

              out
            })
  )

# Put hierarchy in place --------------------------------------------------

# For each geometry, each ID will have to order both THEMES together to know
# which theme to show up first, + intra-theme which VARIBLES to show first
pe_theme_order <-
  map(set_names(names(pe_var_hierarchy)), ~{
    
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
    
    data <- pe_var_hierarchy[[.x]]
    
    data <- 
    map(names(data), function(var_code) {
      names(data[[var_code]]) <- c("ID", "CSDUID", var_code, 
                                   paste0(var_code, "_island_percentile"),
                                   "island_rank",
                                   paste0(var_code, "_region_percentile"), 
                                   "region_rank")
      data[[var_code]][, c(1:4, 6)]
    }) |> reduce(left_join, by = c("ID", "CSDUID"))
    
    data |>
      select(ID, contains("percentile")) |> 
      pivot_longer(-ID) |>
      transmute(ID, 
                group = ifelse(str_detect(name, "island_percentile"), 
                               "island", "region"),
             var_code = str_remove(name, 
                                   "_island_percentile|_region_percentile"),
             percentile = value) |> 
      filter(!is.na(percentile)) |> 
      mutate(max_or_min = abs(0.5 - percentile)) |> 
      left_join(select(place_ex_variables, var_code, theme), 
                by = c("var_code")) |> 
      group_by(ID, theme, group) |> 
      summarize(standout_score = mean(max_or_min), .groups = "drop") |> 
      group_by(ID, group) |> 
      arrange(-standout_score) |> 
      mutate(theme_order = row_number()) |> 
      ungroup() |> 
      mutate(standout = case_when(standout_score > 0.4 ~ "Extreme outlier",
                                  standout_score > 0.3 ~ "Outlier",
                                  TRUE ~ "Typical"))
  })

pe_variable_order <- 
  map(set_names(names(pe_var_hierarchy)), ~{
    
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
    
    data <- pe_var_hierarchy[[.x]]
    
    data <- 
      map(names(data), function(var_code) {
        names(data[[var_code]]) <- c("ID", "CSDUID", var_code, 
                                     paste0(var_code, "_island_percentile"),
                                     "island_rank",
                                     paste0(var_code, "_region_percentile"), 
                                     "region_rank")
        data[[var_code]][, c(1:4, 6)]
      }) |> reduce(left_join, by = c("ID", "CSDUID"))
    
    data |>
      select(ID, contains("percentile")) |> 
      pivot_longer(-ID) |>
      transmute(ID, 
                group = ifelse(str_detect(name, "island_percentile"), 
                               "island", "region"),
                var_code = str_remove(name, 
                                      "_island_percentile|_region_percentile"),
                percentile = value) |> 
      filter(!is.na(percentile)) |> 
      mutate(max_or_min = abs(0.5 - percentile)) |> 
      left_join(select(place_ex_variables, var_code, theme), 
                by = c("var_code")) |> 
      group_by(ID, theme, group) |> 
      arrange(-max_or_min) |> 
      mutate(variable_order = row_number()) |> 
      ungroup() |> 
      select(ID, theme, group, var_code, variable_order, theme)
  })


# Split tables by group ---------------------------------------------------

pe_variable_order <- lapply(pe_variable_order, \(x) split(x, x$group)) 
pe_theme_order <- lapply(pe_theme_order, \(x) split(x, x$group)) 


# Add to modules table ----------------------------------------------------

modules <- 
  modules |> 
  add_modules(id = "place_explorer",
              metadata = FALSE)

# Cleanup -----------------------------------------------------------------

rm(basic_percentile_retrieval, min_access_var_code,
   census_max, groups, last_crash_data_year, island_CSDUID,
   ndvi, no2, percentile_calc)
