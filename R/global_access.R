### ACCESS MODULE GLOBALS ######################################################

# Map token
token_access <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                       "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
rv_access <- reactiveValues(poly_selected = NA)

# Dropdown menu
var_list_left_access_1 <- 
  list("All jobs" = "access_jobs_total",
       "Low-skill jobs" = "access_jobs_low",
       "High-skill jobs" = "access_jobs_high",
       "Jobs < $30,000 annually" = "access_jobs_30k",
       "Schools" = "access_schools",
       "Healthcare facilities" = "access_healthcare")

var_list_left_access_2 <- 
  list("Weekday peak" = "pwd",
       "Weekday off-peak" = "opwd",
       "Weekday night" = "nwd",
       "Weekend peak" = "pwe",
       "Weekend off-peak" = "opwe",
       "Weekend night" = "nwe")

# Dropdown menu
var_list_right_access <- 
  list("----" = " ", 
       "Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_prop",
         "Average rent ($)" = "housing_rent_avg_dollar",
         "Average property value ($)" = "housing_value_avg_dollar",
         "Unaffordable housing (%)" = "housing_unafford_prop",
         "Unsuitable housing (%)" = "housing_unsuit_prop"),
       "Income" = list(
         "Median household income ($)" = "inc_median_dollar",
         "Income under $50k (%)" = "inc_50_prop",
         "Income between $50k-$100k (%)" = "inc_100_prop",
         "Income above $100k (%)" = "inc_high_prop"),
       "Immigration and ethnicity" = list(
         "Immigrants (%)" =  "iden_imm_prop",
         "New immigrants (%)" = "iden_imm_new_prop",
         "Visible minorities (%)" = "iden_vm_prop"),
       "Transportation" = list(
         "Drive to work (%)" = "trans_car_prop",
         "Walk or cycle to work (%)" = "trans_walk_or_bike_prop",
         "Public transit to work (%)" = "trans_transit_prop",
         "15 minutes to work (%)" = "trans_t_15_prop",
         "15-45 minutes to work (%)" = "trans_t_45_prop",
         "More than 45 minutes to work (%)" = "trans_t_45_plus_prop"),
       "Employment" = list(
         "Managerial and professional occupations (%)" = "emp_professional_prop",
         "Creative occupations (%)" = "emp_professional_prop"),
       "Family" = list(
         "Families with children (%)" = "family_children_prop",
         "One person households (%)" = "family_one_person_prop"),
       "Language" = list(
         "French only (%)" = "lang_french_only_prop",
         "English only (%)" = "lang_eng_only_prop",
         "French and English (%)" = "lang_french_eng_prop",
         "Neither French nor English (%)" = "lang_no_official_prop"),
       "Age" = list(
         "Aged between 0 and 14 (%)" = "age_0_14_prop",
         "Aged between 15 and 64 (%)" = "age_15_64_prop",
         "Aged 65 and above (%)" = "age_65_plus_prop"),
       "Education" = list(
         "Bachelor and above (%)" = "edu_bachelor_above_prop",
         "No certificate, diploma or degree (%)" = "edu_no_degree_prop"))

var_list_right_access[-1] <-
  var_list_right_access[-1] %>%
  purrr::modify_depth(2, paste0, "_", current_census)

access_colour <- c("#EDF8E9CC", "#BAE4B3CC", "#74C476CC", "#31A354CC", "#006D2CCC")

# Max values for map bins
access_categories <- 
  c("access_jobs_total", "access_jobs_low", "access_jobs_high", 
    "access_jobs_30k", "access_schools", "access_healthcare")

access_max <- 
  purrr::map_dbl(access_categories, ~{
    CT |> 
      st_drop_geometry() |> 
      select(starts_with(.x) & !contains("q3")) |> 
      as.matrix() |> 
      max(na.rm = TRUE)
  }) |> 
  setNames(access_categories)

get_break <- function(max_val) {
  break_val <- max_val / 5
  round_ten <- round(break_val / (10 ^ ceiling(log10(break_val)))) == 1
  digits <- ceiling(log10(break_val))
  if_else(round_ten, 10 ^ digits, 5 * 10 ^ (digits - 1))
}

colour_access <-
  tibble(category = access_categories,
         max_val = access_max) |> 
  mutate(val_1 = get_break(max_val),
         val_2 = val_1 * 2,
         val_3 = val_1 * 3,
         val_4 = val_1 * 4,
         val_5 = val_1 * 5) |> 
  select(-max_val) |> 
  tidyr::pivot_longer(val_1:val_5) |> 
  left_join(tibble(name =  c("val_1", "val_2", "val_3", "val_4", "val_5"),
                   fill = access_colour), by = "name") |> 
  select(-name)

colour_absolute <- 
  tibble(fill_val = 1:5,
         fill = access_colour)
