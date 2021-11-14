#### Access data setup #########################################################

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

load("dev/data/tt_data_long.Rdata")

points <- 
  read_csv("dev/data/tt_points.csv", col_types = "dcdd") |> 
  select(GeoUID, CT) |> 
  mutate(CT = if_else(nchar(CT) == 7, paste0(CT, ".00"), CT))

feeds <- list.files("dev/data/tt_matrices")

# tt_matrix <-
#   map2_dfr(list(1:7, 8:14, 15:21, c(22, 24, 26, 28, 30, 32, 34),
#                 c(23, 25, 27, 29, 31, 33, 35), 36:42),
#            c("Weekend peak 12h00", "Weekday off-peak 14h00",
#              "Weekend off-peak 18h00", "Weekday night 22h00",
#              "Weekend night 22h00", "Weekday peak 8h00"), ~{
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

tt_matrix <- qread("dev/data/tt_matrix.qs")


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
  select(origin = CT, destination, travel_time) |> 
  left_join(points, by = c("destination" = "GeoUID")) |> 
  select(origin, destination = CT, travel_time)


# Add variable explanations -----------------------------------------------

var_exp <- 
  var_exp %>% 
  add_row(
    var_code = "access_jobs_total_pwd",
    var_name = "Total jobs (weekday peak)",
    explanation = 
      "the total number of jobs accessible within 30 minutes at weekday peak service") %>%
  add_row(
    var_code = "access_jobs_total_opwd",
    var_name = "Total jobs (weekday off-peak)",
    explanation = 
      "the total number of jobs accessible within 30 minutes at weekday off-peak service") %>%
  add_row(
    var_code = "access_jobs_total_nwd",
    var_name = "Total jobs (weekday night)",
    explanation = 
      "the total number of jobs accessible within 30 minutes at weekday night service") %>%
  add_row(
    var_code = "access_jobs_total_pwe",
    var_name = "Total jobs (weekend peak)",
    explanation = 
      "the total number of jobs accessible within 30 minutes at weekend peak service") %>%
  add_row(
    var_code = "access_jobs_total_opwe",
    var_name = "Total jobs (weekend off-peak)",
    explanation = 
      "the total number of jobs accessible within 30 minutes at weekend off-peak service") %>%
  add_row(
    var_code = "access_jobs_total_nwe",
    var_name = "Total jobs (weekend night)",
    explanation = 
      "the total number of jobs accessible within 30 minutes at weekend night service") %>%
  add_row(
    var_code = "access_jobs_low_pwd",
    var_name = "Low-skill jobs (weekday peak)",
    explanation = 
      "the number of low-skill jobs accessible within 30 minutes at weekday peak service") %>%
  add_row(
    var_code = "access_jobs_low_opwd",
    var_name = "Low-skill jobs (weekday off-peak)",
    explanation = 
      "the number of low-skill jobs accessible within 30 minutes at weekday off-peak service") %>%
  add_row(
    var_code = "access_jobs_low_nwd",
    var_name = "Low-skill jobs (weekday night)",
    explanation = 
      "the number of low-skill jobs accessible within 30 minutes at weekday night service") %>%
  add_row(
    var_code = "access_jobs_low_pwe",
    var_name = "Low-skill jobs (weekend peak)",
    explanation = 
      "the number of low-skill jobs accessible within 30 minutes at weekend peak service") %>%
  add_row(
    var_code = "access_jobs_low_opwe",
    var_name = "Low-skill jobs (weekend off-peak)",
    explanation = 
      "the number of low-skill jobs accessible within 30 minutes at weekend off-peak service") %>%
  add_row(
    var_code = "access_jobs_low_nwe",
    var_name = "Low-skill jobs (weekend night)",
    explanation = 
      "the number of low-skill jobs accessible within 30 minutes at weekend night service") %>%
  add_row(
    var_code = "access_jobs_high_pwd",
    var_name = "High-skill jobs (weekday peak)",
    explanation = 
      "the number of high-skill jobs accessible within 30 minutes at weekday peak service") %>%
  add_row(
    var_code = "access_jobs_high_opwd",
    var_name = "High-skill jobs (weekday off-peak)",
    explanation = 
      "the number of high-skill jobs accessible within 30 minutes at weekday off-peak service") %>%
  add_row(
    var_code = "access_jobs_high_nwd",
    var_name = "High-skill jobs (weekday night)",
    explanation = 
      "the number of high-skill jobs accessible within 30 minutes at weekday night service") %>%
  add_row(
    var_code = "access_jobs_high_pwe",
    var_name = "High-skill jobs (weekend peak)",
    explanation = 
      "the number of high-skill jobs accessible within 30 minutes at weekend peak service") %>%
  add_row(
    var_code = "access_jobs_high_opwe",
    var_name = "High-skill jobs (weekend off-peak)",
    explanation = 
      "the number of high-skill jobs accessible within 30 minutes at weekend off-peak service") %>%
  add_row(
    var_code = "access_jobs_high_nwe",
    var_name = "High-skill jobs (weekend night)",
    explanation = 
      "the number of high-skill jobs accessible within 30 minutes at weekend night service") %>%
  add_row(
    var_code = "access_jobs_30k_pwd",
    var_name = "Low-income jobs (weekday peak)",
    explanation = 
      "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday peak service") %>%
  add_row(
    var_code = "access_jobs_30k_opwd",
    var_name = "Low-income jobs (weekday off-peak)",
    explanation = 
      "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday off-peak service") %>%
  add_row(
    var_code = "access_jobs_30k_nwd",
    var_name = "Low-income jobs (weekday night)",
    explanation = 
      "the number of jobs paying less than $30,000 accessible within 30 minutes at weekday night service") %>%
  add_row(
    var_code = "access_jobs_30k_pwe",
    var_name = "Low-income jobs (weekend peak)",
    explanation = 
      "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend peak service") %>%
  add_row(
    var_code = "access_jobs_30k_opwe",
    var_name = "Low-income jobs (weekend off-peak)",
    explanation = 
      "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend off-peak service") %>%
  add_row(
    var_code = "access_jobs_30k_nwe",
    var_name = "Low-income jobs (weekend night)",
    explanation = 
      "the number of jobs paying less than $30,000 accessible within 30 minutes at weekend night service") %>%
  add_row(
    var_code = "access_schools_pwd",
    var_name = "Schools (weekday peak)",
    explanation = 
      "the number of schools accessible within 30 minutes at weekday peak service") %>%
  add_row(
    var_code = "access_schools_opwd",
    var_name = "Schools (weekday off-peak)",
    explanation = 
      "the number of schools accessible within 30 minutes at weekday off-peak service") %>%
  add_row(
    var_code = "access_schools_nwd",
    var_name = "Schools (weekday night)",
    explanation = 
      "the number of schools accessible within 30 minutes at weekday night service") %>%
  add_row(
    var_code = "access_schools_pwe",
    var_name = "Schools (weekend peak)",
    explanation = 
      "the number of schools accessible within 30 minutes at weekend peak service") %>%
  add_row(
    var_code = "access_schools_opwe",
    var_name = "Schools (weekend off-peak)",
    explanation = 
      "the number of schools accessible within 30 minutes at weekend off-peak service") %>%
  add_row(
    var_code = "access_schools_nwe",
    var_name = "Schools (weekend night)",
    explanation = 
      "the number of schools accessible within 30 minutes at weekend night service") %>%
  add_row(
    var_code = "access_healthcare_pwd",
    var_name = "Healthcare (weekday peak)",
    explanation = 
      "the number of healthcare facilities accessible within 30 minutes at weekday peak service") %>%
  add_row(
    var_code = "access_healthcare_opwd",
    var_name = "Healthcare (weekday off-peak)",
    explanation = 
      "the number of healthcare facilities accessible within 30 minutes at weekday off-peak service") %>%
  add_row(
    var_code = "access_healthcare_nwd",
    var_name = "Healthcare (weekday night)",
    explanation = 
      "the number of healthcare facilities within 30 minutes at weekday night service") %>%
  add_row(
    var_code = "access_healthcare_pwe",
    var_name = "Healthcare (weekend peak)",
    explanation = 
      "the number of healthcare facilities accessible within 30 minutes at weekend peak service") %>%
  add_row(
    var_code = "access_healthcare_opwe",
    var_name = "Healthcare (weekend off-peak)",
    explanation = 
      "the number of healthcare facilities accessible within 30 minutes at weekend off-peak service") %>%
  add_row(
    var_code = "access_healthcare_nwe",
    var_name = "Healthcare (weekend night)",
    explanation = 
      "the number of healthcare facilities accessible within 30 minutes at weekend night service")
  

# Clean up ----------------------------------------------------------------

rm(access, data_long, feeds, points)

# To save output, run dev/build_data.R, which calls this script
