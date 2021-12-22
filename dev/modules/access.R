#### Access data setup #########################################################

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

load("dev/data/tt_data_long.Rdata")

points <- 
  read_csv("dev/data/tt_points.csv", col_types = "dcdd") |> 
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
  read_sf("dev/data/transit_shp/mtl_metroline.shp") %>% 
  distinct(LIGNE, .keep_all = T) %>% 
  mutate(LIGNE = as.character(LIGNE),
         line = case_when(LIGNE == 1 ~ "green",
                          LIGNE == 2 ~ "orange",
                          LIGNE == 4 ~ "yellow",
                          LIGNE == 5 ~ "blue"),
         fill = case_when(LIGNE == 1 ~ "#069037",
                          LIGNE == 2 ~ "#f07d05",
                          LIGNE == 4 ~ "#fad706",
                          LIGNE == 5 ~ "#057bc4")) %>% 
  select(line, fill, geometry) %>% 
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
  rename_with(~paste0("access_", .x), jobs_total_pwe:healthcare_pwd)



# Data testing ------------------------------------------------------------

data_testing(list("access" = access))


# Add to existing geographies ---------------------------------------------

CT <-
  CT |> 
  left_join(access, by = c("ID" = "GeoUID")) |> 
  relocate(geometry, .after = last_col()) |> 
  mutate(across(starts_with("access"), ntile, n = 3, .names = "{.col}_q3"), 
         .before = geometry) |> 
  st_set_agr("constant")
  

# Process travel time matrix ----------------------------------------------

tt_matrix <- 
  tt_matrix |> 
  left_join(points, by = c("origin" = "GeoUID")) |> 
  select(timing, origin = CT, destination, travel_time) |> 
  left_join(points, by = c("destination" = "GeoUID")) |> 
  select(timing, origin, destination = CT, travel_time)



# Meta testing ------------------------------------------------------------

meta_testing()


# Add variable explanations -----------------------------------------------

variables <- 
variables |>
  add_variables(
    var_code = "access_jobs_total_pwd",
    var_title = "Total jobs (weekday peak)",
    var_short = "Total WKP",
    explanation = "the total number of jobs accessible within 30 minutes at weekday peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_total_opwd",
    var_title = "Total jobs (weekday off-peak)",
    var_short = "Total WKOP",
    explanation = "the total number of jobs accessible within 30 minutes at weekday off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_total_nwd",
    var_title = "Total jobs (weekday night)",
    var_short = "Total WKN",
    explanation = "the total number of jobs accessible within 30 minutes at weekday night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_total_pwe",
    var_title = "Total jobs (weekend peak)",
    var_short = "Total WEP",
    explanation = "the total number of jobs accessible within 30 minutes at weekend peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_total_opwe",
    var_title = "Total jobs (weekend off-peak)",
    var_short = "Total WEOP",
    explanation = "the total number of jobs accessible within 30 minutes at weekend off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_total_nwe",
    var_title = "Total jobs (weekend night)",
    var_short = "Total WEN",
    explanation = "the total number of jobs accessible within 30 minutes at weekend night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_low_pwd",
    var_title = "Low-skill jobs (weekday peak)",
    var_short = "Low-skill WKP",
    explanation = "the number of low-skill jobs accessible within 30 minutes at weekday peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_low_opwd",
    var_title = "Low-skill jobs (weekday off-peak)",
    var_short = "Low-skill WKOP",
    explanation = "the number of low-skill jobs accessible within 30 minutes at weekday off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_low_nwd",
    var_title = "Low-skill jobs (weekday night)",
    var_short = "Low-skill WKN",
    explanation = "the number of low-skill jobs accessible within 30 minutes at weekday night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_low_pwe",
    var_title = "Low-skill jobs (weekend peak)",
    var_short = "Low-skill WEP",
    explanation = "the number of low-skill jobs accessible within 30 minutes at weekend peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_low_opwe",
    var_title = "Low-skill jobs (weekend off-peak)",
    var_short = "Low-skill WEOP",
    explanation = "the number of low-skill jobs accessible within 30 minutes at weekend off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_low_nwe",
    var_title = "Low-skill jobs (weekend night)",
    var_short = "Low-skill WEN",
    explanation = "the number of low-skill jobs accessible within 30 minutes at weekend night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_high_pwd",
    var_title = "High-skill jobs (weekday peak)",
    var_short = "Hi-skill WKP",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekday peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_high_opwd",
    var_title = "High-skill jobs (weekday off-peak)",
    var_short = "Hi-skill WKOP",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekday off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_high_nwd",
    var_title = "High-skill jobs (weekday night)",
    var_short = "Hi-skill WKN",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekday night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_high_pwe",
    var_title = "High-skill jobs (weekend peak)",
    var_short = "Hi-skill WEP",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekend peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_high_opwe",
    var_title = "High-skill jobs (weekend off-peak)",
    var_short = "Hi-skill WEOP",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekend off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_high_nwe",
    var_title = "High-skill jobs (weekend night)",
    var_short = "Hi-skill WEN",
    explanation = "the number of high-skill jobs accessible within 30 minutes at weekend night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_30k_pwd",
    var_title = "Low-income jobs (weekday peak)",
    var_short = "Low-inc WKP",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_30k_opwd",
    var_title = "Low-income jobs (weekday off-peak)",
    var_short = "Low-inc WKOP",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_30k_nwd",
    var_title = "Low-income jobs (weekday night)",
    var_short = "Low-inc WKN",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_30k_pwe",
    var_title = "Low-income jobs (weekend peak)",
    var_short = "Low-inc WEP",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_30k_opwe",
    var_title = "Low-income jobs (weekend off-peak)",
    var_short = "Low-inc WEOP",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_jobs_30k_nwe",
    var_title = "Low-income jobs (weekend night)",
    var_short = "Low-inc WEN",
    explanation = "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_schools_pwd",
    var_title = "Schools (weekday peak)",
    var_short = "Schools WKP",
    explanation = "the number of schools accessible within 30 minutes at weekday peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_schools_opwd",
    var_title = "Schools (weekday off-peak)",
    var_short = "Schools WKOP",
    explanation = "the number of schools accessible within 30 minutes at weekday off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_schools_nwd",
    var_title = "Schools (weekday night)",
    var_short = "Schools WKN",
    explanation = "the number of schools accessible within 30 minutes at weekday night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_schools_pwe",
    var_title = "Schools (weekend peak)",
    var_short = "Schools WEP",
    explanation = "the number of schools accessible within 30 minutes at weekend peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_schools_opwe",
    var_title = "Schools (weekend off-peak)",
    var_short = "Schools WEOP",
    explanation = "the number of schools accessible within 30 minutes at weekend off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_schools_nwe",
    var_title = "Schools (weekend night)",
    var_short = "Schools WEN",
    explanation = "the number of schools accessible within 30 minutes at weekend night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_healthcare_pwd",
    var_title = "Healthcare (weekday peak)",
    var_short = "Healthcare WKP",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekday peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_healthcare_opwd",
    var_title = "Healthcare (weekday off-peak)",
    var_short = "Healthcare WKOP",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekday off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_healthcare_nwd",
    var_title = "Healthcare (weekday night)",
    var_short = "Healthcare WKN",
    explanation = "the number of healthcare facilities within 30 minutes at weekday night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_healthcare_pwe",
    var_title = "Healthcare (weekend peak)",
    var_short = "Healthcare WEP",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekend peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_healthcare_opwe",
    var_title = "Healthcare (weekend off-peak)",
    var_short = "Healthcare WEOP",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekend off-peak service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  ) |>
  add_variables(
    var_code = "access_healthcare_nwe",
    var_title = "Healthcare (weekend night)",
    var_short = "Healthcare WEN",
    explanation = "the number of healthcare facilities accessible within 30 minutes at weekend night service",
    category = NA,
    private = FALSE,
    dates = NA,
    scales = "CT",
    breaks_q3 = NA,
    breaks_q5 = NA,
    source = "TKTK"
  )


# Clean up ----------------------------------------------------------------

rm(access, data_long, points)

# To save output, run dev/build_data.R, which calls this script
