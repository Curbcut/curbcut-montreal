#### Crash data setup ##########################################################

# This script relies on objects created in dev/census.R

suppressPackageStartupMessages(library(lubridate))


# Get data ----------------------------------------------------------------

# Crash dataframe

crash <- 
  read_sf("dev/data/collisions_routieres/collisions_routieres.shp") |>
  st_transform(4326) |>
  st_set_agr("constant") |> 
  mutate(type = case_when(CD_GENRE_A == 32 ~ "ped",
                          CD_GENRE_A == 31 ~ "cyc",
                          CD_GENRE_A != c(32, 31) ~ "other")) |>
  mutate(injury_total = (NB_BLESSES + NB_BLESS_1)) |>
  select(date = DT_ACCDN, street = RUE_ACCDN, type, seriousness = GRAVITE, 
         death_total = NB_MORTS, death_ped = NB_DECES_P, death_cyc = NB_DECES_V, 
         injury_total, inj_ped = NB_BLESS_2, inj_cyc = NB_BLESS_4,
         day = JR_SEMN_AC, hour = HEURE_ACCD, geometry)

# Traffic activity
# 
# traffic_files <- 
#   list.files("dev/data/traffic_activity") |> 
#   (\(x) paste0("dev/data/traffic_activity/", x))()
# 
# traffic_files_names <- 
#   list.files("dev/data/traffic_activity") |> 
#   str_replace("comptages", "traffic") |> 
#   str_remove_all("_vehicules_cyclistes_pietons|.csv")
# 
# traffic <- 
#   map2(set_names(traffic_files_names), traffic_files, ~{
#     .x <- 
#       read_csv(.y) |> 
#       select(-(Approche_Nord:Localisation_Y), -Code_Banque)
#   }) |> 
#   reduce(rbind) |> 
#   # Sometimes . somtimes , for these ID. They normally use ,
#   mutate(Nom_Intersection = case_when(Nom_Intersection == "Décarie / Van Horne inter. Est" ~ "Décarie / Van Horne inter, Est",
#                                       Nom_Intersection == "Décarie / Van Horne inter. Ouest" ~ "Décarie / Van Horne inter, Ouest",
#                                       Nom_Intersection == "Crémazie / Saint-Michel inter. Nord-Ouest" ~ "Crémazie / Saint-Michel inter, Nord-Ouest",
#                                       Nom_Intersection == "Rita-Levi-Montalcini" ~ "Rita-Levi-Montalcini / Maurice-Duplessis",
#                                       TRUE ~ Nom_Intersection)) |> 
#   # Id_Intersection doubled for different intersection.
#   mutate(Id_Intersection = case_when(Nom_Intersection == "rue Dobrin / boulevard Thimens int. Ouest" ~ 99999,
#                                      TRUE ~ Id_Intersection)) |> 
#   # Transportation type
#   filter(!Description_Code_Banque %in% c("Non Utilise", "Illegaux")) |> 
#   mutate(type = case_when(Description_Code_Banque %in% c("Autos", "Bus", "Camions", "Camions articules", "Camions legers", "Camions Lourds",
#                                                          "Camions porteurs", "Ecoliers", "Motos") ~ "motor",
#                           Description_Code_Banque == "Pietons" ~ "ped",
#                           Description_Code_Banque == "Velos" ~ "cyc",
#                           TRUE ~ "other"))  |> 
#   filter(type == "motor") |> 
#   select(-Description_Code_Banque, -type)
# 
# # Count
# traffic_count <- 
# traffic |> 
#   mutate(ID = row_number()) |> 
#   select(ID, NBLT:WBRT) |> 
#   pivot_longer(-ID) |> 
#   group_by(ID) |> 
#   summarize(count_motorized = sum(value)) |> 
#   pull(count_motorized)
# 
# traffic <- 
# traffic |> 
#   mutate(count_motorized = traffic_count) |>
#   select(-(NBLT:WBRT)) |> 
#   group_by(Id_Reference, Id_Intersection, Nom_Intersection, Date, Periode,
#            Heure, Minute, Seconde, Longitude, Latitude) |> 
#   summarize(count_motorized = sum(count_motorized)) |> 
#   ungroup()
# 
# # Compute yearly average / intersection
# traffic <-
#   traffic |> 
#   # Every row is a period of 15 minutes
#   group_by(intersection = Nom_Intersection, date = Date,
#            Longitude, Latitude) |>
#   # Get a 15m average on a particular intersection and day
#   summarize(count_motorized = sum(count_motorized)/n()) |> 
#   ungroup() |> 
#   # Multiply by 4*24, for a daily number
#   mutate(count_motorized = count_motorized * 4 * 24 * 365) |> 
#   group_by(intersection, year = year(date), Longitude, Latitude) |> 
#   summarize(daily_avg_motorized_count = mean(count_motorized)) |> 
#   ungroup()
# 
# # Get geometry and create a buffer for crashes
# # Geometry operations on a smaller part of the df, saves time
# intersection_geoms <- 
# traffic |> 
#   distinct(intersection, Longitude, Latitude) |> 
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |> 
#   mutate(buffer_50m = st_buffer(geometry, 50))
# 
# traffic <- 
# traffic |> 
#   left_join(intersection_geoms, ., by = "intersection") |> 
#   select(-Longitude, -Latitude) |> 
#   st_as_sf()
# 
# intersection_buffer <- 
#   intersection_geoms |> 
#   st_drop_geometry() |> st_as_sf()
# 
# crashes_in_count_buffer <-
#   st_join(crash, intersection_buffer)
#   
# crashes_in_count_buffer <- 
#   crashes_in_count_buffer |> 
#   st_drop_geometry() |> 
#   filter(!is.na(intersection)) |> 
#   group_by(year = year(date), type, intersection) |> 
#   count(name = "crash_count") |> 
#   ungroup()
# 
# traffic <- 
# traffic |> 
#   select(-buffer_50m) |> 
#   left_join(crashes_in_count_buffer, by = c("intersection", "year")) |> 
#   filter(!is.na(crash_count)) |> 
#   group_by(intersection, year, type) |> 
#   mutate(crash_100k = 100000 * crash_count / daily_avg_motorized_count)
#   
  

# Aggregate data to geometries --------------------------------------------

process_crash <- function(x) {
  
  island_csduid <- c("2466007", "2466023_1",  "2466023_10", "2466023_11",
                     "2466023_12", "2466023_13", "2466023_14", "2466023_15", 
                     "2466023_16", "2466023_17", "2466023_18", "2466023_19",
                     "2466023_2", "2466023_3", "2466023_4", "2466023_5",  
                     "2466023_6", "2466023_7", "2466023_8", "2466023_9",
                     "2466032", "2466047", "2466058", "2466062", "2466087", 
                     "2466092", "2466097", "2466102", "2466107", "2466112",
                     "2466117", "2466127", "2466142", "2466072", "2466023")
  
  crash |> 
    st_transform(32618) |> 
    st_join(st_transform(x, 32618)) |> 
    st_drop_geometry() |> 
    count(ID, year = year(date), type) |> 
    group_by(ID, year) |> 
    summarize(type = c(type, "total"), n = c(n, sum(n, na.rm = TRUE)),
              .groups = "drop") |> 
    pivot_wider(id_cols = "ID",
                names_from = c("type", "year"),
                names_prefix = "crash_",
                names_sep = "_",
                values_from = n) |> 
    # select(-contains("NA")) |> 
    full_join(select(x, any_of(c("ID", "CSDUID")), population), by = "ID") |> 
    # Make sure that a missing geometry shows up as 0. Missing means no crash.
    filter(!is.na(ID)) %>%
    {if (nrow(.) == nrow(borough))
      filter(., ID %in% island_csduid)
      else filter(., CSDUID %in% island_csduid)} |>
    arrange(ID) |> 
    st_as_sf() |> 
    rename_with(~paste0(., "_count"), starts_with("crash")) |> 
    mutate(across(starts_with("crash"), 
                  .fns = list(
                    sqkm = ~{1000000 * .x / 
                        units::drop_units(st_area(geometry))},
                    per1k = ~{1000 * .x / population}),
                  .names = "{str_remove(.col, '_count')}_{.fn}"), 
           .before = geometry) |> 
    mutate(across(starts_with("crash"), ~replace(., is.na(.), 0))) |>
    mutate(across(starts_with("crash"), ~replace(., is.infinite(.), 0))) |>
    rename_with(~paste0(str_remove(., "_\\d{4}"),
                        str_extract(., "_\\d{4}")), starts_with("crash")) |>
    select(-population, -any_of(c("CSDUID"))) |>
    st_drop_geometry()
}

crash_results <- map(list("borough" = borough, "CT" = CT, "DA" = DA, 
                          "grid" = grid), process_crash)

# Add breaks --------------------------------------------------------------

crash_results <- map(crash_results, add_q3)
crash_q3 <- map(crash_results, get_breaks_q3)
crash_q5 <- map(crash_results, get_breaks_q5)
crash_results <- map2(crash_results, crash_q5, ~bind_cols(.x, add_q5(.x, .y)))

# Data testing ------------------------------------------------------------

data_testing(crash_results)


# Join to geometries ------------------------------------------------------

join_crash <- function(x, join_results) {
  x |> 
    left_join(join_results, by = "ID") |> 
    relocate(starts_with("crash"), .before = geometry) |> 
    st_set_agr("constant")
}

borough <- join_crash(borough, crash_results$borough)
CT <- join_crash(CT, crash_results$CT)
DA <- join_crash(DA, crash_results$DA)
grid <- join_crash(grid, crash_results$grid)

building <- 
  building |> 
  left_join(select(as_tibble(DA), ID, starts_with("crash_")),
            by = c("DAUID" = "ID")) |>
  relocate(geometry, .after = last_col())

street <- 
  street |> 
  left_join(select(as_tibble(DA), ID, starts_with("crash_")),
            by = c("DAUID" = "ID")) |>
  relocate(geometry, .after = last_col())


# Add additional fields to crash ------------------------------------------

crash <- 
  crash |> 
  arrange(date) |> 
  mutate(ID = seq_along(date), .before = date) |> 
  mutate(year = year(date), .after = date) |> 
  mutate(fill = case_when(type == "ped" ~ "#91BD9AEE",
                          type == "cyc" ~ "#6C83B5EE",
                          type == "other" ~ "#F39D60EE",
                          TRUE ~ "#E8E8E8EE"), .before = geometry)


# Meta testing ------------------------------------------------------------

meta_testing()


# Add to variables table --------------------------------------------------

var_list <- 
  crash_results |> 
  map(~names(select(.x, -ID, -contains(c("q3", "q5"))))) |> 
  unlist() |> 
  unique()

var_list_no_dates <- str_remove(var_list, "_\\d{4}$") |> unique()

# Get breaks_q3
breaks_q3_active <-
  map2(set_names(var_list), str_extract(var_list, "\\d{4}$"),  function(var_name, year) {
    map2_dfr(crash_q3, names(crash_results), function(x, scale) {
      if (nrow(x) > 0) x |> mutate(scale = scale, date = year, rank = 0:3,
                                   .before = everything())}) |> 
      select(scale, date, rank, var = all_of(var_name))})

names(breaks_q3_active) <- str_remove(names(breaks_q3_active), "_\\d{4}$")

breaks_q3_active <-
  map(set_names(var_list_no_dates), ~{
    breaks_q3_active[names(breaks_q3_active) == .x] |> 
      reduce(rbind)
  })

# Get breaks_q5
breaks_q5_active <-
  map2(set_names(var_list), str_extract(var_list, "\\d{4}$"),  function(var_name, year) {
    map2_dfr(crash_q5, names(crash_results), function(x, scale) {
      if (nrow(x) > 0) x |> mutate(scale = scale, date = year, rank = 0:5,
                                   .before = everything())}) |> 
      select(scale, date, rank, var = all_of(var_name))})

names(breaks_q5_active) <- str_remove(names(breaks_q5_active), "_\\d{4}$")

breaks_q5_active <- 
  map(set_names(var_list_no_dates), ~{
    breaks_q5_active[names(breaks_q5_active) == .x] |> 
      reduce(rbind)
  })

variables <- 
  variables |>
  add_variables(
    var_code = "crash_cyc_count",
    var_title = "Collisions (cyclists)",
    var_short = "Cyclists",
    explanation = "the total number of car collisions involving cyclists",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_cyc_count,
    breaks_q5 = breaks_q5_active$crash_cyc_count,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_other_count",
    var_title = "Collisions (other)",
    var_short = "Other",
    explanation = "the total number of car collisions involving neither pedestrians or cyclists",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_other_count,
    breaks_q5 = breaks_q5_active$crash_other_count,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_ped_count",
    var_title = "Collisions (pedestrians)",
    var_short = "Pedestrian",
    explanation = "the total number of car collisions involving pedestrians",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_ped_count,
    breaks_q5 = breaks_q5_active$crash_ped_count,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_total_count",
    var_title = "Collisions",
    var_short = "Total",
    explanation = "the total number of car collisions",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_total_count,
    breaks_q5 = breaks_q5_active$crash_total_count,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_cyc_per1k",
    var_title = "Collisions per 1,000 (cyclists)",
    var_short = "Crash /1k cyc.",
    explanation = "the total number of car collisions involving cyclists per 1,000 residents",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_cyc_per1k,
    breaks_q5 = breaks_q5_active$crash_cyc_per1k,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_other_per1k",
    var_title = "Collisions per 1,000 (other)",
    var_short = "Crash /1k other",
    explanation = "the total number of car collisions involving neither pedestrians or cyclists per 1,000 residents",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_other_per1k,
    breaks_q5 = breaks_q5_active$crash_other_per1k,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_ped_per1k",
    var_title = "Collisions per 1,000 (pedestrians)",
    var_short = "Crash /1k ped.",
    explanation = "the total number of car collisions involving pedestrians per 1,000 residents",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_ped_per1k,
    breaks_q5 = breaks_q5_active$crash_ped_per1k,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_total_per1k",
    var_title = "Collisions per 1,000",
    var_short = "Crash /1k total",
    explanation = "the total number of car collisions per 1,000 residents",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_total_per1k,
    breaks_q5 = breaks_q5_active$crash_total_per1k,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_cyc_sqkm",
    var_title = "Collisions per sq km (cyclists)",
    var_short = "Crash /sqkm cyc",
    explanation = "the total number of car collisions involving cyclists per square kilometre",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_cyc_sqkm,
    breaks_q5 = breaks_q5_active$crash_cyc_sqkm,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_other_sqkm",
    var_title = "Collisions per sq km (other)",
    var_short = "Crash /sqkm oth",
    explanation = "the total number of car collisions involving neither pedestrians or cyclists per square kilometre",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_other_sqkm,
    breaks_q5 = breaks_q5_active$crash_other_sqkm,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_ped_sqkm",
    var_title = "Collisions per sq km (pedestrians)",
    var_short = "Crash /sqkm ped",
    explanation = "the total number of car collisions involving pedestrians per square kilometre",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_ped_sqkm,
    breaks_q5 = breaks_q5_active$crash_ped_sqkm,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_total_sqkm",
    var_title = "Collisions per sq km",
    var_short = "Crash /sqkm tot",
    explanation = "the total number of car collisions per square kilometre",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = breaks_q3_active$crash_total_sqkm,
    breaks_q5 = breaks_q5_active$crash_total_sqkm,
    source = "spvm_saaq")


# Clean-up ----------------------------------------------------------------

rm(crash_results, process_crash, join_crash#, 
   # traffic_count, traffic_files, traffic_files_names
   )

