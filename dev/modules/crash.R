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
    group_by(ID, year = year(date), type) |> 
    summarize(n = n(), .groups = "drop") |> 
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



# Add variable explanations -----------------------------------------------

var_exp <- 
  var_exp %>% 
  add_row(
    var_code = "crash_cyc",
    var_name = "Total collisions (cyclists)",
    explanation = 
      "the total number of car collisions involving cyclists") %>%
  add_row(
    var_code = "crash_other",
    var_name = "Total collisions (other)",
    explanation = 
      "the total number of car collisions involving neither pedestrians or cyclists") %>%
  add_row(
    var_code = "crash_ped",
    var_name = "Total collisions (pedestrians)",
    explanation = 
      "the total number of car collisions involving pedestrians") %>%
  add_row(
    var_code = "crash_total",
    var_name = "Total collisions",
    explanation = 
      "the total number of car collisions") %>%
  add_row(
    var_code = "crash_cyc_per1k",
    var_name = "Total collisions per 1,000 (cyclists)",
    explanation = 
      "the total number of car collisions involving cyclists per 1,000 residents") %>%
  add_row(
    var_code = "crash_other_per1k",
    var_name = "Total collisions per 1,000 (other)",
    explanation = 
      "the total number of car collisions involving neither pedestrians or cyclists per 1,000 residents") %>%
  add_row(
    var_code = "crash_ped_per1k",
    var_name = "Total collisions per 1,000 (pedestrians)",
    explanation = 
      "the total number of car collisions involving pedestrians per 1,000 residents") %>%
  add_row(
    var_code = "crash_total_per1k",
    var_name = "Total collisions per 1,000",
    explanation = 
      "the total number of car collisions per 1,000 residents") %>%
  add_row(
    var_code = "crash_cyc_sqkm",
    var_name = "Total collisions per sq km (cyclists)",
    explanation = 
      "the total number of car collisions involving cyclists per square kilometre") %>%
  add_row(
    var_code = "crash_other_sqkm",
    var_name = "Total collisions per sq km (other)",
    explanation = 
      "the total number of car collisions involving neither pedestrians or cyclists per square kilometre") %>%
  add_row(
    var_code = "crash_ped_sqkm",
    var_name = "Total collisions per sq km (pedestrians)",
    explanation = 
      "the total number of car collisions involving pedestrians per square kilometre") %>%
  add_row(
    var_code = "crash_total_sqkm",
    var_name = "Total collisions per sq km",
    explanation = 
      "the total number of car collisions per square kilometre")
