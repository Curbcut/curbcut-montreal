#### Crash data setup ##########################################################

# This script relies on objects created in dev/census.R

suppressPackageStartupMessages(library(lubridate))


# Get data ----------------------------------------------------------------

crash <- 
  read_sf("dev/data/collisions_routieres/collisions_routieres.shp") %>%
  st_transform(4326) %>%
  st_set_agr("constant") |> 
  mutate(type = case_when(CD_GENRE_A == 32 ~ "ped",
                          CD_GENRE_A == 31 ~ "cyc",
                          CD_GENRE_A != c(32, 31) ~ "other")) %>%
  mutate(injury_total = (NB_BLESSES + NB_BLESS_1)) %>%
  select(date = DT_ACCDN, street = RUE_ACCDN, type, seriousness = GRAVITE, 
         death_total = NB_MORTS, death_ped = NB_DECES_P, death_cyc = NB_DECES_V, 
         injury_total, inj_ped = NB_BLESS_2, inj_cyc = NB_BLESS_4,
         day = JR_SEMN_AC, hour = HEURE_ACCD, geometry)


# Aggregate data to census tables -----------------------------------------

process_crash <- function(x) {
  join_results <- 
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
    select(-contains("NA"))
  
  x |> 
    left_join(join_results, by = "ID") |> 
    relocate(starts_with("crash"), .before = geometry) |> 
    mutate(across(starts_with("crash"), 
                  .fns = list(
                    sqkm = ~{1000000 * .x / 
                        units::drop_units(st_area(geometry))},
                    per1k = ~{1000 * .x / population}),
                  .names = "{.col}_{.fn}"), .before = geometry) |> 
    mutate(across(starts_with("crash"), ntile, n = 3, .names = "{.col}_q3"), 
           .before = geometry) |> 
    rename_with(~paste0(str_remove(., "_\\d{4}"), 
                        str_extract(., "_\\d{4}")), starts_with("crash")) |> 
    st_set_agr("constant")
}

crash_results <- map(list(borough, CT, DA, grid), process_crash)

borough <- crash_results[[1]]
CT <- crash_results[[2]]
DA <- crash_results[[3]]
grid <- crash_results[[4]]

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

rm(crash_results, process_crash)


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


# Add variable explanations -----------------------------------------------

variables <- 
  variables |>
  add_variables(
    var_code = "crash_cyc",
    var_title = "Collisions (cyclists)",
    var_short = "Cyclists",
    explanation = "the total number of car collisions involving cyclists",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_other",
    var_title = "Collisions (other)",
    var_short = "Other",
    explanation = "the total number of car collisions involving neither pedestrians or cyclists",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_ped",
    var_title = "Collisions (pedestrians)",
    var_short = "Pedestrian",
    explanation = "the total number of car collisions involving pedestrians",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_total",
    var_title = "Collisions",
    var_short = "Total",
    explanation = "the total number of car collisions",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_cyc_per1k",
    var_title = "Collisions per 1,000 (cyclists)",
    var_short = "per 1k cyc.",
    explanation = "the total number of car collisions involving cyclists per 1,000 residents",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_other_per1k",
    var_title = "Collisions per 1,000 (other)",
    var_short = "per 1k other",
    explanation = "the total number of car collisions involving neither pedestrians or cyclists per 1,000 residents",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_ped_per1k",
    var_title = "Collisions per 1,000 (pedestrians)",
    var_short = "per 1k ped.",
    explanation = "the total number of car collisions involving pedestrians per 1,000 residents",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_total_per1k",
    var_title = "Collisions per 1,000",
    var_short = "per 1k total",
    explanation = "the total number of car collisions per 1,000 residents",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_cyc_sqkm",
    var_title = "Collisions per sq km (cyclists)",
    var_short = "per sqkm cyc",
    explanation = "the total number of car collisions involving cyclists per square kilometre",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_other_sqkm",
    var_title = "Collisions per sq km (other)",
    var_short = "per sqkm oth",
    explanation = "the total number of car collisions involving neither pedestrians or cyclists per square kilometre",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_ped_sqkm",
    var_title = "Collisions per sq km (pedestrians)",
    var_short = "per sqkm ped",
    explanation = "the total number of car collisions involving pedestrians per square kilometre",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq") |>
  add_variables(
    var_code = "crash_total_sqkm",
    var_title = "Collisions per sq km",
    var_short = "per sqkm tot",
    explanation = "the total number of car collisions per square kilometre",
    category = NA,
    private = FALSE,
    dates = as.character(2012:2019),
    scales = c("borough", "building", "CT", "DA", "grid", "street"),
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "spvm_saaq")
