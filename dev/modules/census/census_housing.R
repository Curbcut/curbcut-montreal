#### Census housing data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_housing <- tibble(
  var_code = character(),
  vec_2016 = character(),
  vec_2011 = character(),
  vec_2006 = character(),
  vec_2001 = character(),
  vec_1996 = character(),
  include = logical(),
  var_title = character(),
  var_short = character(),
  explanation = character(),
  category = character(),
  private = logical()
  )
    
census_housing <- 
  census_housing |> 
  add_row(
    var_code = "housing_tenant_pct",
    vec_2016 = "v_CA16_4838",
    vec_2011 = "v_CA11N_2254",
    vec_2006 = "v_CA06_103",
    vec_2001 = "v_CA01_100",
    vec_1996 = "v_CA1996_1683",
    include = TRUE,
    var_title = "Tenant-occupied (%)",
    var_short = "Tenant (%)",
    explanation = "the percentage of private dwellings occupied by tenants",
    private = FALSE) |> 
  add_row(
    var_code = "housing_rent_avg_dollar",
    vec_2016 = "v_CA16_4901",
    vec_2011 = "v_CA11N_2292",
    vec_2006 = "v_CA06_2050",
    vec_2001 = "v_CA01_1667",
    vec_1996 = "v_CA1996_1701",
    include = TRUE,
    var_title = "Average rent ($)",
    var_short = "TKTK",
    explanation = "the average rent paid by tenants per month",
    private = FALSE) |>
  add_row(
    var_code = "housing_repairs_pct",
    vec_2016 = "v_CA16_4872",
    vec_2011 = "v_CA11N_2232",
    vec_2006 = "v_CA06_108",
    vec_2001 = "v_CA01_104",
    vec_1996 = "v_CA1996_1687",
    include = TRUE,
    var_title = "Housing requiring major repairs (%)",
    var_short = "TKTK",
    explanation = "the percentage of households living in dwellings requiring major repairs",
    private = FALSE) |>
  add_row(
    var_code = "housing_value_avg_dollar",
    vec_2016 = "v_CA16_4896",
    vec_2011 = "v_CA11N_2287",
    vec_2006 = "v_CA06_2054",
    vec_2001 = "v_CA01_1674",
    vec_1996 = "v_CA1996_1681",
    include = TRUE,
    var_title = "Average property value ($)",
    var_short = "TKTK",
    explanation = "the average value of owner-occupied dwellings",
    private = FALSE) |>
  add_row(
    var_code = "housing_unafford_pct",
    vec_2016 = "v_CA16_4888",
    include = TRUE,
    var_title = "Unaffordable housing (%)",
    var_short = "TKTK",
    explanation = "the percentage of dwellings for which residents pay more than 30% of income on housing costs",
    private = FALSE) |>
  add_row(
    var_code = "housing_unsuit_pct",
    vec_2016 = "v_CA16_4861",
    vec_2011 = "v_CA11N_2276",
    include = TRUE,
    var_title = "Unsuitable housing (%)",
    var_short = "TKTK",
    explanation = "the percentage of households living in accommodations without enough bedrooms according to the National Occupancy Standard",
    private = FALSE) |>
  add_row(
    var_code = "housing_stress_renter_pct",
    vec_2016 = "v_CA16_4899",
    vec_2011 = "v_CA11N_2290",
    include = TRUE,
    var_title = "Renter housing stress (%)",
    var_short = "TKTK",
    explanation = "the percentage of renter households that spend more than 30% of their income on shelter costs",
    private = FALSE) |>
  add_row(
    var_code = "housing_stress_owner_pct",
    vec_2016 = "v_CA16_4892",
    vec_2011 = "v_CA11N_2283",
    include = TRUE,
    var_title = "Owner housing stress (%)",
    var_short = "TKTK",
    explanation = "the percentage of owner households that spend more than 30% of their income on shelter costs",
    private = FALSE) |>
  add_row(
    var_code = "housing_mobility_one_pct",
    vec_2016 = "v_CA16_6698",
    vec_2011 = "v_CA11N_1723",
    vec_2006 = "v_CA06_453",
    vec_2001 = "v_CA01_383",
    vec_1996 = "v_CA1996_1387",
    include = TRUE,
    var_title = "One-year housing mobility (%)",
    var_short = "TKTK",
    explanation = "the percentage of households that have moved in the past year",
    private = FALSE) |>
  add_row(
    var_code = "housing_mobility_five_pct",
    vec_2016 = "v_CA16_6725",
    vec_2011 = "v_CA11N_1750",
    vec_2006 = "v_CA06_462",
    vec_2001 = "v_CA01_392",
    vec_1996 = "v_CA1996_1396",
    include = TRUE,
    var_title = "Five-year housing mobility (%)",
    var_short = "TKTK",
    explanation = "the percentage of households that have moved in the past five years",
    private = FALSE)
  

# Gather data -------------------------------------------------------------

data_to_add <- census_data_gather(census_housing, scales, years,
                                  parent_vectors = c("housing_value_avg_dollar" = "v_CA01_1670"))


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

new_vars <- add_vars(data_to_add, census_housing, breaks_q3, breaks_q5)
variables <- bind_rows(variables, new_vars)
