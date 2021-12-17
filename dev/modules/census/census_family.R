#### Census family data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_family <- tibble(
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

add_row_fam <- function(data, var_code, vec_2016, vec_2011, vec_2006, vec_2001,
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
          private = private
  )
}
    
census_family <- 
  census_family |> 
  add_row_fam(
    var_code = "family_children_pct",
    vec_2016 = "v_CA16_507",
    vec_2011 = c("v_CA11F_129", "v_CA11F_119", "v_CA11F_125"),
    vec_2006 = c("v_CA06_65", "v_CA06_59", "v_CA06_69"),
    vec_2001 = c("v_CA01_63", "v_CA01_57", "v_CA01_67"),
    vec_1996 = NA,
    var_title = "Families with children (%)",
    var_short = "TKTK",
    explanation = "the percentage of census families with children out of total households",
    private = FALSE) |> 
  add_row_fam(
    var_code = "family_one_person_pct",
    vec_2016 = "v_CA16_510",
    vec_2011 = "v_CA11F_157",
    vec_2006 = "v_CA06_89",
    vec_2001 = "v_CA01_87",
    vec_1996 = "v_CA1996_98",
    var_title = "One person households (%)",
    var_short = "TKTK",
    explanation = "the percentage of one person households out of total households",
    private = FALSE)
  

# Gather data -------------------------------------------------------------

data_to_add <- 
  add_census_data(census_family, scales, years, parent_vectors = 
                    c("family_children_pct" = "v_CA11F_115",
                      "family_children_pct" = "v_CA06_55",
                      "family_children_pct" = "v_CA01_53"))


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
