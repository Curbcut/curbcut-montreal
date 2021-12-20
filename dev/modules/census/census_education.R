#### Census education data ####################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_vec <- 
  census_vec |> 
  add_row_census_vec(
    var_code = "edu_bachelor_above_pct",
    vec_2016 = "v_CA16_5078",
    vec_2011 = "v_CA11N_1792",
    vec_2006 = c("v_CA06_1240", "v_CA06_1254", "v_CA06_1268"),
    vec_2001 = "v_CA01_1397",
    vec_1996 = "v_CA1996_1360",
    var_title = "Bachelor and above (%)",
    var_short = "Bachelor+",
    explanation = paste0("the percentage of the population aged 15 and over ",
                         "holding a degree at bachelor level or above"),
    private = FALSE) |> 
  add_row_census_vec(
    var_code = "edu_no_degree_pct",
    vec_2016 = "v_CA16_5054",
    vec_2011 = "v_CA11N_1774",
    vec_2006 = c("v_CA06_1235", "v_CA06_1249", "v_CA06_1263"),
    vec_2001 = "v_CA01_1387",
    vec_1996 = "v_CA1996_1350",
    var_title = "No certificate, diploma or degree (%)",
    var_short = "No degree",
    explanation = paste0("the percentage of the population aged 15 and over ",
                         "with no certificate, diploma or degree"),
    private = FALSE)


# Parent vectors ----------------------------------------------------------

parent_vectors  <- c(parent_vectors,
                     c("edu_no_degree_pct" = c("v_CA06_1234", "v_CA06_1248", "v_CA06_1262"),
                       "edu_bachelor_above_pct" = c("v_CA06_1234", "v_CA06_1248", "v_CA06_1262")))
