#### Census family data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_family <- tibble(
  var_code = character(),
  vec_2016 = character(),
  vec_2011 = character(),
  vec_2006 = character(),
  vec_2001 = character(),
  vec_1996 = character(),
  var_title = character(),
  var_short = character(),
  explanation = character(),
  category = character(),
  private = logical()
  )

census_family <- 
  census_family |> 
  add_row(
    var_code = "lang_french_only_pct",
    vec_2016 = "v_CA16_518",
    vec_2011 = "v_CA11F_557",
    vec_2006 = "v_CA06_245",
    vec_2001 = "v_CA01_215",
    vec_1996 = "v_CA1996_312",
    var_title = "French only (%)",
    var_short = "TKTK",
    explanation = "the percentage of individuals that only know French as an official language",
    private = FALSE) |> 
  add_row(
    var_code = "lang_eng_only_pct",
    vec_2016 = "v_CA16_515",
    vec_2011 = "v_CA11F_554",
    vec_2006 = "v_CA06_244",
    vec_2001 = "v_CA01_214",
    vec_1996 = "v_CA1996_311",
    var_title = "English only (%)",
    var_short = "TKTK",
    explanation = "the percentage of individuals that only know English as an official language",
    private = FALSE) |> 
  add_row(
    var_code = "ang_french_eng_pct",
    vec_2016 = "v_CA16_521",
    vec_2011 = "v_CA11F_560",
    vec_2006 = "v_CA06_246",
    vec_2001 = "v_CA01_216",
    vec_1996 = "v_CA1996_313",
    var_title = "French and English (%)",
    var_short = "TKTK",
    explanation = "the percentage of individuals that know both official languages (French and English)",
    private = FALSE) |> 
  add_row(
    var_code = "lang_no_official_pct",
    vec_2016 = "v_CA16_524",
    vec_2011 = "v_CA11F_563",
    vec_2006 = "v_CA06_247",
    vec_2001 = "v_CA01_217",
    vec_1996 = "v_CA1996_314",
    var_title = "Neither French nor English (%)",
    var_short = "TKTK",
    explanation = "the percentage of individuals that do not know either of the official languages (French or English)",
    private = FALSE)
  

# Gather data -------------------------------------------------------------

data_to_add <- 
  add_census_data(census_family, scales, years)


# Assign data -------------------------------------------------------------

borough <- 
  borough |> 
  left_join(data_to_add$borough, by = "ID") |> 
  relocate(geometry, .after = last_col())

CT <- 
  CT |> 
  left_join(data_to_add$CT, by = "ID") |> 
  relocate(geometry, .after = last_col())

DA <- 
  DA |> 
  left_join(data_to_add$DA, by = "ID") |> 
  relocate(centroid, buffer, geometry, .after = last_col())

grid <-
  grid |>
  left_join(data_to_add$grid, by = "ID") |>
  relocate(geometry, .after = last_col())


# Add to variables table --------------------------------------------------

new_vars <- add_vars(data_to_add, census_family, breaks_q3, breaks_q5)
variables <- bind_rows(variables, new_vars)
