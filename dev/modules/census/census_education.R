#### Census edu data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_edu <- tibble(
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

add_row_edu <- function(data, var_code, vec_2016, vec_2011, vec_2006, vec_2001,
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

cancensus::list_census_vectors("CA06") |> 
  filter(vector %in% c("v_CA06_1240", "v_CA06_1254", "v_CA06_1268"))

census_edu <- 
  census_edu |> 
  add_row_edu(
    var_code = "edu_bachelor_above_pct",
    vec_2016 = "v_CA16_5078",
    vec_2011 = "v_CA11N_1792",
    vec_2006 = c("v_CA06_1240", "v_CA06_1254", "v_CA06_1268"),
    vec_2001 = "v_CA01_1397",
    vec_1996 = "v_CA1996_1360",
    var_title = "Bachelor and above (%)",
    var_short = "TKTK",
    explanation = paste0("the percentage of the population aged 15 and over ",
                         "holding a degree at bachelor level or above"),
    private = FALSE) |> 
  add_row_edu(
    var_code = "edu_no_degree_pct",
    vec_2016 = "v_CA16_5054",
    vec_2011 = "v_CA11N_1774",
    vec_2006 = c("v_CA06_1235", "v_CA06_1249", "v_CA06_1263"),
    vec_2001 = "v_CA01_1387",
    vec_1996 = "v_CA1996_1350",
    var_title = "No certificate, diploma or degree (%)",
    var_short = "TKTK",
    explanation = paste0("the percentage of the population aged 15 and over ",
                         "with no certificate, diploma or degree"),
    private = FALSE)


# Gather data -------------------------------------------------------------

data_to_add <- 
  add_census_data(census_edu, scales, years,
                  parent_vectors = c("edu_no_degree_pct" = c("v_CA06_1234", "v_CA06_1248", "v_CA06_1262"),
                                     "edu_bachelor_above_pct" = c("v_CA06_1234", "v_CA06_1248", "v_CA06_1262")))


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

new_vars <- add_vars(data_to_add, census_edu, breaks_q3, breaks_q5)
variables <- bind_rows(variables, new_vars)
