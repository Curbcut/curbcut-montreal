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
    var_code = "family_children_pct",
    vec_2016 = "v_CA16_507",
    vec_2011 = NA,
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Families with children (%)",
    var_short = "TKTK",
    explanation = "the percentage of census families with children out of total households",
    private = FALSE) |> 
  add_row(
    var_code = "family_one_person_pct",
    vec_2016 = "v_CA16_510",
    vec_2011 = NA,
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "One person households (%)",
    var_short = "TKTK",
    explanation = "the percentage of one person households out of total households",
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
