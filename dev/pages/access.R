#### Access data setup #########################################################

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

load("dev/data/tt_data_long.Rdata")

points <- 
  read_csv("dev/data/tt_points.csv", col_types = "dcdd", progress = FALSE) |> 
  select(GeoUID, CT) |> 
  mutate(CT = if_else(nchar(CT) == 7, paste0(CT, ".00"), CT))

# feeds <- list.files("dev/data/tt_matrices")
# 
# tt_matrix <-
#   map2_dfr(list(1:7, 8:14, 15:21, c(22, 24, 26, 28, 30, 32, 34),
#                 c(23, 25, 27, 29, 31, 33, 35), 36:42),
#            c("pwe", "opwd", "opwe", "nwd", "nwe", "pwd"), ~{
#     read_csv(paste0("dev/data/tt_matrices/", feeds[.x]),
#              show_col_types = FALSE) |>
#       mutate(timing = .y)}) |>
#   group_by(timing, origin, destination) |>
#   filter(travel_time == min(travel_time, na.rm = TRUE)) |>
#   slice(1) |>
#   ungroup() |>
#   select(timing, origin, destination, travel_time)
# 
# qsave(tt_matrix, file = "dev/data/tt_matrix.qs")
# 
# rm(feeds)

tt_matrix <- qread("dev/data/tt_matrix.qs")

metro_lines <- 
  read_sf("dev/data/transit_shp/mtl_metroline.shp") |> 
  distinct(LIGNE, .keep_all = T) |> 
  mutate(LIGNE = as.character(LIGNE),
         line = case_when(LIGNE == 1 ~ "green",
                          LIGNE == 2 ~ "orange",
                          LIGNE == 4 ~ "yellow",
                          LIGNE == 5 ~ "blue"),
         fill = case_when(LIGNE == 1 ~ "#069037",
                          LIGNE == 2 ~ "#f07d05",
                          LIGNE == 4 ~ "#fad706",
                          LIGNE == 5 ~ "#057bc4")) |> 
  select(line, fill, geometry) |> 
  st_transform(4326)


# Process access table ----------------------------------------------------

access <-
  data_long |> 
  filter(scenario == "Baseline") |> 
  filter(job_type != "health") |> 
  select(GeoUID:value) |> 
  select(-scenario) |> 
  mutate(job_type = case_when(
    job_type == "total" ~ "jobs_total",
    job_type == "low-skill" ~ "jobs_low",
    job_type == "high-skill" ~ "jobs_high",
    job_type == "jobs30k" ~ "jobs_30k",
    job_type == "Education" ~ "schools",
    job_type == "Healthcare" ~ "healthcare"
  )) |> 
  pivot_wider(names_from = c(job_type, time), values_from = value) |> 
  rename_with(~paste0("access_", .x), jobs_total_pwe:healthcare_pwd) |> 
  rename_with(~paste0(.x, "_count"), 
              access_jobs_total_pwe:access_healthcare_pwd) |> 
  rename(ID = GeoUID)

# Add CTs not part of the access df and add it to a list
access <- 
  list(CMA_CT = bind_rows(access, map_dfr(CMA_CT$ID[!CMA_CT$ID %in% access$ID], 
                          ~{tibble(ID = .x)})))


# Add breaks --------------------------------------------------------------

access <- calculate_breaks(access)


# Add to existing geographies ---------------------------------------------

assign_tables(access)
  

# Process travel time matrix ----------------------------------------------

tt_matrix <- 
  tt_matrix |> 
  left_join(points, by = c("origin" = "GeoUID")) |> 
  select(timing, origin = CT, destination, travel_time) |> 
  left_join(points, by = c("destination" = "GeoUID")) |> 
  select(timing, origin, destination = CT, travel_time)

# Make sure that every combination is there
tt_matrix <- 
  map_dfr(unique(tt_matrix$timing), function(time) {
    expand.grid(CT$ID, CT$ID) |> 
      as_tibble() |> 
      mutate(timing = time)
  }) |> 
  rename(origin = Var1,
         destination = Var2) |> 
  relocate(timing, .before = origin) |> 
  left_join(tt_matrix, by = c("timing", "origin", "destination"))

tt_matrix <- 
  tt_matrix |> 
  pivot_wider(names_from = origin, values_from = c(travel_time))


# Meta testing ------------------------------------------------------------

# meta_testing()


# Add variable explanations -----------------------------------------------

# Get breaks_q3
breaks_q3_access <-
  imap_dfr(access$tables_q3, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, rank = 0:3,
                                 .before = 1)})

# Get breaks_q5
breaks_q5_access <-
  imap_dfr(access$tables_q5, \(x, scale) {
    if (nrow(x) > 0) x |> mutate(scale = scale, rank = 0:5,
                                 .before = 1)})

variables <- 
  variables |>
  add_variables(
    var_code = "access_jobs_total_pwd_count",
    var_title = "Total jobs (weekday peak)",
    var_short = "Total WKP",
    explanation = paste0("the total number of jobs accessible within 30 ",
                         "minutes at weekday peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_total_pwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_total_pwd_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))
  ) |>
  add_variables(
    var_code = "access_jobs_total_opwd_count",
    var_title = "Total jobs (weekday off-peak)",
    var_short = "Total WKOP",
    explanation = paste0("the total number of jobs accessible within 30 ",
                         "minutes at weekday off-peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_total_opwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_total_opwd_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))
  ) |>
  add_variables(
    var_code = "access_jobs_total_nwd_count",
    var_title = "Total jobs (weekday night)",
    var_short = "Total WKN",
    explanation = paste0("the total number of jobs accessible within 30 ",
                         "minutes at weekday night service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_total_nwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_total_nwd_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))
    ) |>
  add_variables(
    var_code = "access_jobs_total_pwe_count",
    var_title = "Total jobs (weekend peak)",
    var_short = "Total WEP",
    explanation = paste0("the total number of jobs accessible within 30 ",
                         "minutes at weekend peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_total_pwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_total_pwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))
  ) |>
  add_variables(
    var_code = "access_jobs_total_opwe_count",
    var_title = "Total jobs (weekend off-peak)",
    var_short = "Total WEOP",
    explanation = paste0("the total number of jobs accessible within 30 ",
                         "minutes at weekend off-peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_total_opwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_total_opwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))
  ) |>
  add_variables(
    var_code = "access_jobs_total_nwe_count",
    var_title = "Total jobs (weekend night)",
    var_short = "Total WEN",
    explanation = paste0("the total number of jobs accessible within 30 ",
                         "minutes at weekend night service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_total_nwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_total_nwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))
  ) |>
  add_variables(
    var_code = "access_jobs_low_pwd_count",
    var_title = "Low-skill jobs (weekday peak)",
    var_short = "Low-skill WKP",
    explanation = paste0("the number of low-skill jobs accessible within ",
                         "30 minutes at weekday peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_low_pwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_low_pwd_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))
    ) |>
  add_variables(
    var_code = "access_jobs_low_opwd_count",
    var_title = "Low-skill jobs (weekday off-peak)",
    var_short = "Low-skill WKOP",
    explanation = paste0("the number of low-skill jobs accessible within ",
                         "30 minutes at weekday off-peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_low_opwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_low_opwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_low_nwd_count",
    var_title = "Low-skill jobs (weekday night)",
    var_short = "Low-skill WKN",
    explanation = paste0("the number of low-skill jobs accessible within ",
                         "30 minutes at weekday night service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_low_nwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_low_nwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_low_pwe_count",
    var_title = "Low-skill jobs (weekend peak)",
    var_short = "Low-skill WEP",
    explanation = paste0("the number of low-skill jobs accessible within ",
                         "30 minutes at weekend peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_low_pwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_low_pwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_low_opwe_count",
    var_title = "Low-skill jobs (weekend off-peak)",
    var_short = "Low-skill WEOP",
    explanation = paste0("the number of low-skill jobs accessible within 30 ",
                         "minutes at weekend off-peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_low_opwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_low_opwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_low_nwe_count",
    var_title = "Low-skill jobs (weekend night)",
    var_short = "Low-skill WEN",
    explanation = paste0("the number of low-skill jobs accessible within 30 ",
                         "minutes at weekend night service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_low_nwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_low_nwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_high_pwd_count",
    var_title = "High-skill jobs (weekday peak)",
    var_short = "Hi-skill WKP",
    explanation = paste0("the number of high-skill jobs accessible within 30 ",
                         "minutes at weekday peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_high_pwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_high_pwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_high_opwd_count",
    var_title = "High-skill jobs (weekday off-peak)",
    var_short = "Hi-skill WKOP",
    explanation = paste0("the number of high-skill jobs accessible within 30 ",
                         "minutes at weekday off-peak service"),
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_high_opwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_high_opwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_high_nwd_count",
    var_title = "High-skill jobs (weekday night)",
    var_short = "Hi-skill WKN",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekday night service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_high_nwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_high_nwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_high_pwe_count",
    var_title = "High-skill jobs (weekend peak)",
    var_short = "Hi-skill WEP",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekend peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_high_pwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_high_pwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_high_opwe_count",
    var_title = "High-skill jobs (weekend off-peak)",
    var_short = "Hi-skill WEOP",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekend off-peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_high_opwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_high_opwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_high_nwe_count",
    var_title = "High-skill jobs (weekend night)",
    var_short = "Hi-skill WEN",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekend night service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_high_nwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_high_nwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_30k_pwd_count",
    var_title = "Low-income jobs (weekday peak)",
    var_short = "Low-inc WKP",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_30k_pwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_30k_pwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_30k_opwd_count",
    var_title = "Low-income jobs (weekday off-peak)",
    var_short = "Low-inc WKOP",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday off-peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_30k_opwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_30k_opwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_30k_nwd_count",
    var_title = "Low-income jobs (weekday night)",
    var_short = "Low-inc WKN",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday night service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_30k_nwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_30k_nwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_30k_pwe_count",
    var_title = "Low-income jobs (weekend peak)",
    var_short = "Low-inc WEP",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_30k_pwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_30k_pwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_30k_opwe_count",
    var_title = "Low-income jobs (weekend off-peak)",
    var_short = "Low-inc WEOP",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend off-peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_30k_opwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_30k_opwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_jobs_30k_nwe_count",
    var_title = "Low-income jobs (weekend night)",
    var_short = "Low-inc WEN",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend night service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_jobs_30k_nwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_jobs_30k_nwe_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_schools_pwd_count",
    var_title = "Schools (weekday peak)",
    var_short = "Schools WKP",
    explanation = "the number of schools accessible within 30 minutes at weekday peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_schools_pwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_schools_pwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_schools_opwd_count",
    var_title = "Schools (weekday off-peak)",
    var_short = "Schools WKOP",
    explanation = "the number of schools accessible within 30 minutes at weekday off-peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_schools_opwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_schools_opwd_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_schools_nwd_count",
    var_title = "Schools (weekday night)",
    var_short = "Schools WKN",
    explanation = "the number of schools accessible within 30 minutes at weekday night service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_schools_nwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_schools_nwd_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_schools_pwe_count",
    var_title = "Schools (weekend peak)",
    var_short = "Schools WEP",
    explanation = "the number of schools accessible within 30 minutes at weekend peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_schools_pwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_schools_pwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_schools_opwe_count",
    var_title = "Schools (weekend off-peak)",
    var_short = "Schools WEOP",
    explanation = "the number of schools accessible within 30 minutes at weekend off-peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_schools_opwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_schools_opwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_schools_nwe_count",
    var_title = "Schools (weekend night)",
    var_short = "Schools WEN",
    explanation = "the number of schools accessible within 30 minutes at weekend night service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_schools_nwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_schools_nwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_healthcare_pwd_count",
    var_title = "Healthcare (weekday peak)",
    var_short = "Healthcare WKP",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekday peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_healthcare_pwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_healthcare_pwd_count),

    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_healthcare_opwd_count",
    var_title = "Healthcare (weekday off-peak)",
    var_short = "Healthcare WKOP",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekday off-peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_healthcare_opwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_healthcare_opwd_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_healthcare_nwd_count",
    var_title = "Healthcare (weekday night)",
    var_short = "Healthcare WKN",
    explanation = "the number of healthcare facilities within 30 minutes at weekday night service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_healthcare_nwd_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_healthcare_nwd_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_healthcare_pwe_count",
    var_title = "Healthcare (weekend peak)",
    var_short = "Healthcare WEP",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekend peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_healthcare_pwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_healthcare_pwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_healthcare_opwe_count",
    var_title = "Healthcare (weekend off-peak)",
    var_short = "Healthcare WEOP",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekend off-peak service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_healthcare_opwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_healthcare_opwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))  ) |>
  add_variables(
    var_code = "access_healthcare_nwe_count",
    var_title = "Healthcare (weekend night)",
    var_short = "Healthcare WEN",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekend night service",
    category = NA,
    theme = "Transport",
    private = FALSE,
    dates = NA,
    scales = names(access$tables_q3),
    breaks_q3 = select(breaks_q3_access, scale, rank, access_healthcare_nwe_count),
    breaks_q5 = select(breaks_q5_access, scale, rank, access_healthcare_nwe_count),
    source = paste0("Job and population data from Statistics Canada. Travel ",
                    "time calculations from OSM and GTFS."),
    interpolated = list(c(CMA_CT = FALSE))
    )


# Add to modules table ----------------------------------------------------

modules <- 
  modules |> 
  add_modules(id = "access",
              metadata = TRUE,
              dataset_info = 
                paste0("<p>For more information on how accessibility metrics are ",
                       "calculated, see <a href = 'https://conservancy.umn.edu/bitstream/",
                       "handle/11299/199892/CTS13-20_Access-Across-America.pdf'>",
                       "'Access Across America'</a>.</p>"))


# Clean up ----------------------------------------------------------------

rm(access, breaks_q3_access, breaks_q5_access, data_long, points)
