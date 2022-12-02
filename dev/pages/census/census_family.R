#### Census family data ########################################################

# This script relies on objects created in dev/build_data.R and
# dev/modules/census/build_census.R


# Topic vectors -----------------------------------------------------------

census_vec <- 
  census_vec |> 
  add_row_census_vec(
    var_code = "family_children_pct",
    vec_2016 = "v_CA16_507",
    vec_2011 = c("v_CA11F_129", "v_CA11F_119", "v_CA11F_125"),
    vec_2006 = c("v_CA06_65", "v_CA06_59", "v_CA06_69"),
    vec_2001 = c("v_CA01_63", "v_CA01_57", "v_CA01_67"),
    vec_1996 = NA,
    var_title = "Families with children (%)",
    var_short = "With child",
    explanation = "the percentage of census families with children out of total households",
    private = FALSE) |> 
  add_row_census_vec(
    var_code = "family_one_person_pct",
    vec_2016 = "v_CA16_510",
    vec_2011 = "v_CA11F_157",
    vec_2006 = "v_CA06_89",
    vec_2001 = "v_CA01_87",
    vec_1996 = "v_CA1996_98",
    var_title = "Living alone (%)",
    var_short = "Living alone",
    explanation = "the percentage of one person households out of total households",
    private = FALSE)
  

# Parent_vectors ----------------------------------------------------------

parent_vectors <- c(parent_vectors,
                    c("family_children_pct" = "v_CA11F_115",
                      "family_children_pct" = "v_CA06_55",
                      "family_children_pct" = "v_CA01_53",
                      "family_children_pct" = "v_CA16_504",
                      "family_one_person_pct" = "v_CA16_504"))
