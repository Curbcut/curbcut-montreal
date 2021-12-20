#### Census income data #######################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_vec <-
  census_vec |>
  add_row_census_vec(
    var_code = "inc_median_dollar",
    vec_2016 = "v_CA16_2397",
    vec_2011 = "v_CA11N_2562",
    vec_2006 = "v_CA06_2000",
    vec_2001 = "v_CA01_1634",
    vec_1996 = "v_CA1996_1627",
    var_title = "Median household income ($)",
    var_short = "Med. income",
    explanation = "median before-tax household income",
    private = FALSE) |>
  add_row_census_vec(
    var_code = "inc_50_pct",
    vec_2016 = c("v_CA16_2406", "v_CA16_2407", "v_CA16_2408", "v_CA16_2409",
                 paste0("v_CA16_24", 10:15)),
    vec_2011 = paste0("v_CA11N_25", 34:40),
    vec_2006 = paste0("v_CA06_19", 89:93),
    vec_2001 = paste0("v_CA01_16", 22:26),
    vec_1996 = paste0("v_CA1996_16", 15:19),
    var_title = "Income under $50k (%)",
    var_short = "Inc. <$50k",
    explanation = paste0("the percentage of households with an income less ",
                         "then $50,000"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "inc_100_pct",
    vec_2016 = paste0("v_CA16_24", 16:20),
    vec_2011 = paste0("v_CA11N_25", 41:43),
    vec_2006 = paste0("v_CA06_19", 94:98),
    vec_2001 = paste0("v_CA01_16", 27:31),
    vec_1996 = paste0("v_CA1996_16", 20:24),
    var_title = "Income between $50k-$100k (%)",
    var_short = "Inc. $50-100k",
    explanation = paste0("the percentage of households with an income between", 
                         "$50,000 and $100,000"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "inc_high_pct",
    vec_2016 = "v_CA16_2421",
    vec_2011 = "v_CA11N_2546",
    vec_2006 = "v_CA06_1999",
    vec_2001 = "v_CA01_1632",
    vec_1996 = "v_CA1996_1625",
    var_title = "Income above $100k (%)",
    var_short = "Inc. >$100k",
    explanation = paste0("the percentage of households with an income higher ",
                         "than $100,000"),
    private = FALSE) |>
  add_row_census_vec(
    var_code = "inc_limat_pct",
    vec_2016 = "v_CA16_2540",
    vec_2011 = NA,
    vec_2006 = NA,
    vec_2001 = NA,
    vec_1996 = NA,
    var_title = "Prevalence of low income (after-tax) (%)",
    var_short = "Low income",
    explanation = paste0("the prevalence of low income in private households ",
                         "based on the Low-income measure, after-tax (LIM-AT)"),
    private = FALSE)

