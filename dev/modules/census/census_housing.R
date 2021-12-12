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
  
  
# Get empty geometries ----------------------------------------------------

geoms <- get_empty_geometries(scales, years)


# Download data -----------------------------------------------------------

data_raw <- get_census_vectors(census_housing, geoms, scales, years,
                               parent_vectors = c("housing_value_avg_dollar" = "v_CA01_1670"))


# Get aggregation type ----------------------------------------------------

data_aggregation <- get_aggregation_type(census_housing, scales, years)


# Interpolate -------------------------------------------------------------

var_count <- 
  data_aggregation |>
  filter(aggregation == "Additive") %>% 
  pull(var_code)

var_avg <- 
  data_aggregation |>
  filter(aggregation == "Average") %>% 
  pull(var_code)

if (length(c(var_count, var_avg)) != length(census_housing$var_code)) {
  stop("The number of var_count and var_avg isn't the same as the number of ",
       "variables.")
}

data_inter <- interpolate(data_raw, scales, years)


# Swap CSD to borough -----------------------------------------------------

# TKTK THIS NEEDS TO HAPPEN HERE, BEFORE NORMALIZATION!


# Interpolate to building, grid & street ----------------------------------

# TKTK THIS SHOULD HAPPEN NOW AS WELL, SO THAT NORMALIZATION IS MORE PRECISE

# Get units type ----------------------------------------------------------

data_unit <- get_unit_type(census_housing, scales, years)


# Normalize pct variables -------------------------------------------------

data_norm <- normalize(data_inter, census_housing)


# Drop variables which aren't included in final tables --------------------

data_final <- drop_vars(data_norm, census_housing)


# Add q3 and q5 versions --------------------------------------------------

cat_q5 <- get_categories_q5(data_final, census_housing)
data_q3 <- add_q3(data_final)
breaks_q3 <- get_breaks_q3(data_q3, census_housing)
breaks_q5 <- get_breaks_q5(data_final, cat_q5)
data_q5 <- add_q5(data_final, breaks_q5)
data_breaks <- merge_breaks(data_final, data_q3, data_q5)


# Add years ---------------------------------------------------------------

data_years <- add_years(data_breaks, years)


# Finalize output ---------------------------------------------------------

data_to_add <- reduce_years(data_years)

# building <- 
#   building |> 
#   left_join(data_to_add[[1]], by = "ID") |> 
#   relocate(geometry, .after = last_col())

## TKTK DOESN'T WORK UNTIL CSD IS SWITCHED TO BOROUGH!
borough <- 
  borough |> 
  left_join(data_to_add[[1]], by = "ID") |> 
  relocate(geometry, .after = last_col())

CT <- 
  CT |> 
  left_join(data_to_add[[2]], by = "ID") |> 
  relocate(geometry, .after = last_col())

DA <- 
  DA |> 
  left_join(data_to_add[[3]], by = "ID") |> 
  relocate(centroid, buffer, geometry, .after = last_col())

# grid <- 
#   grid |> 
#   left_join(data_to_add[[1]], by = "ID") |> 
#   relocate(geometry, .after = last_col())

# street <- 
#   street |> 
#   left_join(data_to_add[[1]], by = "ID") |> 
#   relocate(geometry, .after = last_col())


# Add to variables table --------------------------------------------------

new_vars <- add_vars(data_to_add, census_housing, breaks_q3, breaks_q5)
variables <- bind_rows(variables, new_vars)
