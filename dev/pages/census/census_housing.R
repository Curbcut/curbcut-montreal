#### Census housing data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_vec <- 
  census_vec |> 
  add_row_census_vec(
    var_code = "housing_tenant_pct",
    vec_2016 = "v_CA16_4838",
    vec_2011 = "v_CA11N_2254",
    vec_2006 = "v_CA06_103",
    vec_2001 = "v_CA01_100",
    vec_1996 = "v_CA1996_1683",
    var_title = "Tenant-occupied (%)",
    var_short = "Tenant",
    explanation = "the percentage of private dwellings occupied by tenants",
    private = FALSE) |> 
  add_row_census_vec(
    var_code = "housing_rent_avg_dollar",
    vec_2016 = "v_CA16_4901",
    vec_2011 = "v_CA11N_2292",
    vec_2006 = "v_CA06_2050",
    vec_2001 = "v_CA01_1667",
    vec_1996 = "v_CA1996_1701",
    var_title = "Average rent ($)",
    var_short = "Avg. rent",
    explanation = "the average rent paid by tenants per month",
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_repairs_pct",
    vec_2016 = "v_CA16_4872",
    vec_2011 = "v_CA11N_2232",
    vec_2006 = "v_CA06_108",
    vec_2001 = "v_CA01_104",
    vec_1996 = "v_CA1996_1687",
    var_title = "Housing requiring major repairs (%)",
    var_short = "Repairs",
    explanation = paste0("the percentage of households living in dwellings ",
                         "requiring major repairs"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_value_avg_dollar",
    vec_2016 = "v_CA16_4896",
    vec_2011 = "v_CA11N_2287",
    vec_2006 = "v_CA06_2054",
    vec_2001 = "v_CA01_1674",
    vec_1996 = "v_CA1996_1681",
    var_title = "Average property value ($)",
    var_short = "Avg. value",
    explanation = "the average value of owner-occupied dwellings",
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_unafford_pct",
    vec_2016 = "v_CA16_4888",
    vec_2011 = NA,
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Unaffordable housing (%)",
    var_short = "Unaffordable",
    explanation = paste0("the percentage of dwellings for which residents pay ",
                         "more than 30% of income on housing costs"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_unsuit_pct",
    vec_2016 = "v_CA16_4861",
    vec_2011 = "v_CA11N_2276",
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Unsuitable housing (%)",
    var_short = "Unsuitable",
    explanation = paste0("the percentage of households living in ",
                         "accommodations without enough bedrooms"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_stress_renter_pct",
    vec_2016 = "v_CA16_4899",
    vec_2011 = "v_CA11N_2290",
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Renter housing stress (%)",
    var_short = "Renter stress",
    explanation = paste0("the percentage of renter households that spend ",
                         "more than 30% of their income on shelter costs"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_stress_owner_pct",
    vec_2016 = "v_CA16_4892",
    vec_2011 = "v_CA11N_2283",
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Owner housing stress (%)",
    var_short = "Owner stress",
    explanation = paste0("the percentage of owner households that spend more ",
                         "than 30% of their income on shelter costs"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_mobility_one_pct",
    vec_2016 = "v_CA16_6698",
    vec_2011 = "v_CA11N_1723",
    vec_2006 = "v_CA06_453",
    vec_2001 = "v_CA01_383",
    vec_1996 = "v_CA1996_1387",
    var_title = "One-year housing mobility (%)",
    var_short = "1-year mob.",
    explanation = paste0("the percentage of households that have moved in ",
                         "the past year"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_mobility_five_pct",
    vec_2016 = "v_CA16_6725",
    vec_2011 = "v_CA11N_1750",
    vec_2006 = "v_CA06_462",
    vec_2001 = "v_CA01_392",
    vec_1996 = "v_CA1996_1396",
    var_title = "Five-year housing mobility (%)",
    var_short = "5-year mob.",
    explanation = paste0("the percentage of households that have moved in the ",
                         "past five years"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "housing_single_detached_pct",
    vec_2016 = "v_CA16_409",
    vec_2011 = "v_CA11F_200",
    vec_2006 = "v_CA06_120",
    vec_2001 = "v_CA01_113",
    vec_1996 = "v_CA1996_108",
    var_title = "Single-detached (%)",
    var_short = "Single-detached",
    explanation = paste0("the percentage of occupied private dwellings that ",
                         "are single-detached houses"),
    private = FALSE)
  

# Parent vector -----------------------------------------------------------

parent_vectors <- c(parent_vectors, 
                    c("housing_value_avg_dollar" = "v_CA01_1670",
                      "housing_mobility_five_pct" = "v_CA1996_1394"))
