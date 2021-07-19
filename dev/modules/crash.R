#### Crash data setup ##########################################################

# This script relies on objects created in dev/census.R

library(lubridate)

# Get data ----------------------------------------------------------------

crash <- 
  read_sf("dev/data/collisions_routieres/collisions_routieres.shp")

crash <- 
  crash %>%
  st_transform(4326) %>%
  st_set_agr("constant") |> 
  mutate(type = case_when(CD_GENRE_A == 32 ~ "ped",
                          CD_GENRE_A == 31 ~ "cyc",
                          CD_GENRE_A != c(32, 31) ~ "other")) %>%
  select(date = DT_ACCDN, street = RUE_ACCDN, type, seriousness = GRAVITE, 
         death_total = NB_MORTS, death_ped = NB_DECES_P, death_cyc = NB_DECES_V, 
         injury_total = NB_BLESSES, inj_ped = NB_BLESS_2, inj_cyc = NB_BLESS_4,
         geometry)


# Aggregate data to census tables -----------------------------------------

process_crash <- function(x) {
  join_results <- 
    crash |> 
    st_join(x) |> 
    st_drop_geometry() |> 
    group_by(ID, year = year(date), type) |> 
    summarize(n = n(), 
              # across(c(death_total:inj_cyc), sum, na.rm = TRUE), 
              .groups = "drop") |> 
    pivot_wider(id_cols = "ID",
                names_from = c("type", "year"),
                names_prefix = "crash_",
                names_sep = "_",
                # values_from = c(n:inj_cyc)
                values_from = n)
  
  x |> 
    left_join(join_results, by = "ID") |> 
    relocate(starts_with("crash"), .before = geometry) |> 
    mutate(across(starts_with("crash"), 
                  ~{.x / units::drop_units(st_area(geometry))},
                  .names = "{.col}_prop_area"),
           across(starts_with("crash"), ~{.x / population}, 
                  .names = "{.col}_prop_pop"), .before = geometry) |> 
    mutate(across(starts_with("crash"), ntile, n = 3, .names = "{.col}_q3"), 
           .before = geometry) %>% 
    st_set_agr("constant")
}

crash_results <- map(list(borough, CT, DA, grid), process_crash)

borough <- crash_results[[1]]
CT <- crash_results[[2]]
DA <- crash_results[[3]]
grid <- crash_results[[4]]

rm(crash_results, process_crash)

names(borough)
