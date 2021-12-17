#### Census housing data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_employment <- tibble(
  var_code = character(),
  vec_2016 = list(),
  vec_2011 = list(),
  vec_2006 = list(),
  vec_2001 = list(),
  vec_1996 = list(),
  var_title = character(),
  var_short = character(),
  explanation = character(),
  category = character(),
  private = logical()
)

add_row_emp <- function(data, var_code, vec_2016, vec_2011, vec_2006, vec_2001,
                        vec_1996, var_title, var_short, explanation, private) {
  add_row(data,
          var_code = var_code,
          vec_2016 = list(vec_2016),
          vec_2011 = list(vec_2011),
          vec_2006 = list(vec_2006),
          vec_2001 = list(vec_2001),
          vec_1996 = list(vec_1996),
          var_title = var_title,
          var_short = var_short,
          explanation = explanation,
          private = private)
}

census_employment <-
  census_employment |>
  add_row_emp(
    var_code = "emp_professional_pct",
    vec_2016 = c("v_CA16_5735", "v_CA16_5738"),
    vec_2011 = c("v_CA11N_2107", "v_CA11N_2110"),
    vec_2006 = c("v_CA06_1021", "v_CA06_1022"),
    vec_2001 = c("v_CA01_1181", "v_CA01_1182"),
    vec_1996 = NA,
    var_title = "Managerial and professional occupations (%)",
    var_short = "Professional",
    explanation = paste0("the percentage of the workforce in professional and ",
                         "managerial occupations, based on the North American ",
                         "Industry Classification System"),
    private = FALSE
  ) |>
  add_row_emp(
    var_code = "emp_creative_pct",
    vec_2016 = c("v_CA16_5726", "v_CA16_5750"),
    vec_2011 = c("v_CA11N_2098", "v_CA11N_2122"),
    vec_2006 = c("v_CA06_1018", "v_CA06_1026"),
    vec_2001 = c("v_CA01_1178", "v_CA01_1186"),
    vec_1996 = NA,
    var_title = "Creative occupations (%)",
    var_short = "Creative",
    explanation = paste0("the percentage of the workforce in artistic and ",
                         "cultural occupations, based on the North American ",
                         "Industry Classification System"),
    private = FALSE)


# Gather data -------------------------------------------------------------

data_to_add <- add_census_data(census_employment, scales, years)


# Assign data -------------------------------------------------------------

borough <-
  borough |>
  left_join(data_to_add[[1]]$borough, by = "ID") |>
  relocate(geometry, .after = last_col())

CT <-
  CT |>
  left_join(data_to_add[[1]]$CT, by = "ID") |>
  relocate(geometry, .after = last_col())

DA <-
  DA |>
  left_join(data_to_add[[1]]$DA, by = "ID") |>
  relocate(centroid, buffer, geometry, .after = last_col())

grid <-
  grid |>
  left_join(data_to_add[[1]]$grid, by = "ID") |>
  relocate(geometry, .after = last_col())


# Add to variables table --------------------------------------------------

variables <- bind_rows(variables, data_to_add[[2]])
rm(census_employment)
