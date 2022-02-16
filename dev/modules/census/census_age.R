#### Census age data #########################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_vec <- 
  census_vec |> 
  add_row_census_vec(
    var_code = "age_0_14_pct",
    vec_2016 = "v_CA16_4",
    vec_2011 = c("v_CA11F_8", "v_CA11F_11", "v_CA11F_14"),
    vec_2006 = c("v_CA06_4", "v_CA06_5", "v_CA06_6", "v_CA06_23", "v_CA06_24", "v_CA06_25"),
    vec_2001 = c("v_CA01_7", "v_CA01_8", "v_CA01_9", "v_CA01_26", "v_CA01_27", "v_CA01_28"),
    vec_1996 = c("v_CA1996_7", "v_CA1996_31", "v_CA1996_8", "v_CA1996_32", "v_CA1996_9", "v_CA1996_33"),
    var_title = "Aged between 0 and 14 (%)",
    var_short = "0-14 yo",
    explanation = "the percentage of the population aged between 0 and 14 years old",
    private = FALSE) |> 
  add_row_census_vec(
    var_code = "age_15_64_pct",
    vec_2016 = "v_CA16_61",
    vec_2011 = paste0("v_CA11F_", c(17,35,38,41,44,47,50,53,56,59)),
    vec_2006 = c(paste0("v_CA06_", 7:16), paste0("v_CA06_", 26:35)),
    vec_2001 = c(paste0("v_CA01_", 10:19), paste0("v_CA01_", 29:38)),
    vec_1996 = c(paste0("v_CA1996_", 15:24), paste0("v_CA1996_", 39:48)),
    var_title = "Aged between 15 and 64 (%)",
    var_short = "15-64 yo",
    explanation = "the percentage of the population aged between 15 and 64 years old",
    private = FALSE) |> 
  add_row_census_vec(
    var_code = "age_65_plus_pct",
    vec_2016 = "v_CA16_244",
    vec_2011 = c(paste0("v_CA11F_", c(62,65,68,71,74))),
    vec_2006 = c(paste0("v_CA06_", 17:21), paste0("v_CA06_", 36:40)),
    vec_2001 = c(paste0("v_CA01_", 20:24), paste0("v_CA01_", 39:43)),
    vec_1996 = c(paste0("v_CA1996_", 25:29), paste0("v_CA1996_", 49:53)),
    var_title = "Aged 65 and above (%)",
    var_short = "65+ yo",
    explanation = "the percentage of the population aged 65 and above",
    private = FALSE)


# Parent vectors ----------------------------------------------------------

parent_vectors <-  c(parent_vectors, 
                   c("age_0_14_pct" = c("v_CA06_3", "v_CA06_22"), 
                     "age_15_64_pct" = c("v_CA06_3", "v_CA06_22"), 
                     "age_65_plus_pct" = c("v_CA06_3", "v_CA06_22"),
                     "age_0_14_pct" = c("v_CA01_6", "v_CA01_25"),
                     "age_15_64_pct" = c("v_CA01_6", "v_CA01_25"),
                     "age_65_plus_pct" = c("v_CA01_6", "v_CA01_25"),
                     "age_0_14_pct" = c("v_CA1996_6", "v_CA1996_30"),
                     "age_15_64_pct" = c("v_CA1996_6", "v_CA1996_30"),
                     "age_65_plus_pct" = c("v_CA1996_6", "v_CA1996_30")))
