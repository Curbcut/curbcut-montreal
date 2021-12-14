#### Census housing data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_income <- tibble(
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

add_row_inc <- function(data, var_code, vec_2016, vec_2011, vec_2006, vec_2001, vec_1996, 
                        var_title, var_short, explanation, private) {
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
    
census_income <-
  census_income |> 
    add_row_inc(
    var_code = "inc_median_dollar",
    vec_2016 = "v_CA16_2397",
    vec_2011 = "v_CA11N_2562",
    vec_2006 = "v_CA06_2000",
    vec_2001 = "v_CA01_1634",
    vec_1996 = "v_CA1996_1627",
    var_title = "Median household income ($)",
    var_short = "TKTK",
    explanation = "median before-tax household income",
    private = FALSE) |> 
    add_row_inc(
    var_code = "inc_50_pct",
    vec_2016 = paste0("v_CA16_24", 06:15),
    vec_2011 = paste0("v_CA11N_25", 34:40),
    vec_2006 = paste0("v_CA06_19", 89:93),
    vec_2001 = paste0("v_CA01_16", 22:26),
    vec_1996 = paste0("v_CA1996_16", 15:19),
    var_title = "Income under $50k (%)",
    var_short = "TKTK",
    explanation = "the percentage of households with an income less then $50,000",
    private = FALSE) |>
  # add_row(
  #   var_code = "inc_100_pct",
  #   vec_2016 = "v_CA16_4901",
  #   vec_2011 = "v_CA11N_2292",
  #   vec_2006 = "v_CA06_2050",
  #   vec_2001 = "v_CA01_1667",
  #   vec_1996 = "v_CA1996_1701",
  #   include = TRUE,
  #   var_title = "Income beetween $50k-$100k (%)",
  #   var_short = "TKTK",
  #   explanation = "the percentage of households with an income between $50,000 and $100,000",
  #   private = FALSE) |>
  add_row_inc(
    var_code = "inc_high_pct",
    vec_2016 = "v_CA16_4872",
    vec_2011 = "v_CA11N_2232",
    vec_2006 = "v_CA06_108",
    vec_2001 = "v_CA01_104",
    vec_1996 = "v_CA1996_1687",
    var_title = "Income above $100k (%)",
    var_short = "TKTK",
    explanation = "the percentage of households with an income higher than $100,000",
    private = FALSE)

# |>
  # add_row_inc(
  #   var_code = "inc_limat_prop",
  #   vec_2016 = "v_CA16_4896",
  #   vec_2011 = "v_CA11N_2287",
  #   vec_2006 = "v_CA06_2054",
  #   vec_2001 = "v_CA01_1674",
  #   vec_1996 = "v_CA1996_1681",
  #   include = TRUE,
  #   var_title = "Prevalence of low income (after-tax) (%)",
  #   var_short = "TKTK",
  #   explanation = "the prevalence of low income in private households based on the Low income measure, after-tax(LIM-AT)",
  #   private = FALSE)
  

# Gather data -------------------------------------------------------------

data_to_add <- census_data_gather(census_income, scales, years,
                                  # For income, the sum of all variables in inc_50_pct must
                                  # have income total as parent.
                                  parent_vectors = c("inc_50_pct" = "v_CA16_2405",
                                                     "inc_50_pct" = "v_CA11N_2533"))


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

new_vars <- add_vars(data_to_add, census_income, breaks_q3, breaks_q5)
variables <- bind_rows(variables, new_vars)
