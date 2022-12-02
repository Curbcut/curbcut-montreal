#### Census employment data ####################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_vec <-
  census_vec |>
  add_row_census_vec(
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
  add_row_census_vec(
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
