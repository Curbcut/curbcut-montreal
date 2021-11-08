#### Census housing data #######################################################

# This script relies on objects created in dev/build_geometries.R and
# dev/modules/census/build_census.R

# Setting variables -------------------------------------------------------

year_census <- "2016"

# Topic vectors -----------------------------------------------------------

census_housing <- tibble(
  var_code = character(),
  vec_2016 = character(),
  vec_2011 = character(),
  vec_2006 = character(),
  vec_1996 = character(),
  var_name = character(),
  explanation = character())
    
census_housing |> 
  add_row(
    var_code = "tenure_households",
    vec_2016 = "v_CA16_4836",
    vec_2011 = "v_CA11N_2252",
    vec_2006 = "v_CA06_101",
    vec_2001 = "v_CA01_96", # also repairs_total
    vec_1996 = "v_CA1996_1678", # also repairs_total,
    explanation = 
  )
    
# 2016
census_housing <- c(
  renter = "v_CA16_4838",
  rent_avg_total = "v_CA16_4897",
  housing_rent_avg_dollar = "v_CA16_4901",
  value_avg_total = "v_CA16_4890",
  major_repairs = "v_CA16_4872",
  repairs_total = "v_CA16_4870",
  housing_value_avg_dollar = "v_CA16_4896",
  housing_unafford_total = "v_CA16_4886",
  housing_unafford = "v_CA16_4888",
  housing_unsuit_total = "v_CA16_4859",
  housing_unsuit = "v_CA16_4861",
  housing_stress_renter_prop = "v_CA16_4899",
  housing_stress_owner_prop = "v_CA16_4892",
  housing_mobility_one = "v_CA16_6698",
  housing_mobility_one_total = "v_CA16_6692",
  housing_mobility_five = "v_CA16_6725",
  housing_mobility_five_total = "v_CA16_6719")

#2011
census_housing <- c(
  renter = "v_CA11N_2254", #NHS
  rent_avg_total = "v_CA11N_2288", #NHS
  housing_rent_avg_dollar = "v_CA11N_2292", #NHS
  value_avg_total = "v_CA11N_2281", #NHS
  major_repairs = "v_CA11N_2232", #NHS
  repairs_total = "v_CA11N_2230", #NHS
  housing_value_avg_dollar = "v_CA11N_2287", #NHS
  housing_unsuit_total = "v_CA11N_2274", #NHS
  housing_unsuit = "v_CA11N_2276", #NHS
  housing_stress_renter_prop = "v_CA11N_2290", #NHS
  housing_stress_owner_prop = "v_CA11N_2283",
  housing_mobility_one = "v_CA11N_1723",
  housing_mobility_one_total = "v_CA11N_1717",
  housing_mobility_five = "v_CA11N_1750",
  housing_mobility_five_total = "v_CA11N_1744")

# 2006
census_housing <- c(
  renter = "v_CA06_103", 
  rent_avg_total = "v_CA06_2049",
  housing_rent_avg_dollar = "v_CA06_2050",
  value_avg_total = "v_CA06_2053", 
  major_repairs = "v_CA06_108", 
  repairs_total = "v_CA06_105", 
  housing_value_avg_dollar = "v_CA06_2054", 
  housing_stress_owner = "v_CA06_2056",
  housing_stress_renter = "v_CA06_2051",
  housing_mobility_one = "v_CA06_453",
  housing_mobility_one_total = "v_CA06_451",
  housing_mobility_five = "v_CA06_462",
  housing_mobility_five_total = "v_CA06_460")

# 2001
census_housing <- c(
  renter = "v_CA01_100", 
  rent_avg_total = "v_CA01_1666", #also housing_stress_renter_total
  housing_rent_avg_dollar = "v_CA01_1667",
  value_avg_total = "v_CA01_1670", #also housing_stress_owner_total
  major_repairs = "v_CA01_104", 
  housing_value_avg_dollar = "v_CA01_1674", 
  housing_stress_owner = "v_CA01_1672",
  housing_stress_renter = "v_CA01_1668",
  housing_mobility_one = "v_CA01_383",
  housing_mobility_one_total = "v_CA01_381",
  housing_mobility_five = "v_CA01_392",
  housing_mobility_five_total = "v_CA01_390")

# 1996
census_housing <- c(
  #renter = "v_CA1996_1683", #also rent_avg_total
  rent_avg_total = "v_CA1996_1683", 
  housing_rent_avg_dollar = "v_CA1996_1701",
  value_avg_total = "v_CA1996_1682", 
  major_repairs = "v_CA1996_1687", 
  #repairs_total = "v_CA1996_1678", 
  housing_value_avg_dollar = "v_CA1996_1681", 
  housing_stress_owner = "v_CA1996_1705",
  housing_stress_renter = "v_CA1996_1702",
  housing_mobility_one = "v_CA1996_1387",
  housing_mobility_one_total = "v_CA1996_1385",
  housing_mobility_five = "v_CA1996_1396",
  housing_mobility_five_total = "v_CA1996_1394")









# Download data -----------------------------------------------------------

source("dev/modules/census/census_retrieval_functions.R")

census_geos <- census_retrieval("CA16", added_var_group = c(census_age, census_family, census_language))

rm(census_housing, census_identity, census_income, census_transport,
   census_age, census_education, census_employment, census_family,
   census_language, vars_to_remove, census_retrieval)


# Helper function for processing data -------------------------------------

process_census_data <- function(data) {
  
  # Process housing
  var_list <- c("housing_tenant_prop", "housing_rent_avg_dollar", 
                "housing_value_avg_dollar", "housing_unafford_prop",
                "housing_unsuit_prop", "housing_repairs_prop",
                "housing_stress_renter_prop", "housing_stress_owner_prop",
                "housing_mobility_one_prop", "housing_mobility_five_prop")
  
  data <- 
    data %>% 
    mutate(housing_tenant_prop = renter / tenure_households,
           housing_unafford_prop = housing_unafford / housing_unafford_total,
           housing_unsuit_prop = housing_unsuit / housing_unsuit_total,
           housing_repairs_prop = major_repairs / repairs_total,
           housing_stress_renter_prop = housing_stress_renter_prop / 100,
           housing_stress_owner_prop = housing_stress_owner_prop / 100,
           housing_mobility_one_prop = housing_mobility_one / housing_mobility_one_total,
           housing_mobility_five_prop = housing_mobility_five / housing_mobility_five_total) %>% 
    select(-c(renter, tenure_households, housing_unafford, 
              housing_unafford_total, housing_unsuit, housing_unsuit_total,
              rent_avg_total, value_avg_total, major_repairs, repairs_total,
              housing_mobility_one, housing_mobility_one_total, housing_mobility_five,
              housing_mobility_five_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  # Process income
  var_list <- c("inc_median_dollar", "inc_50_prop", "inc_100_prop", 
                "inc_high_prop", "inc_limat_prop")
  
  data <- 
    data %>% 
    mutate(inc_50_prop = (inc_5 + inc_10 + inc_15 + inc_20 +
                            inc_25 + inc_30 + inc_35 + inc_40 + 
                            inc_45 + inc_50) / inc_total,
           inc_100_prop = (inc_60 + inc_70 + inc_80 + inc_90 + 
                             inc_100) / inc_total,
           inc_high_prop = inc_high / inc_total,
           inc_limat_prop = inc_limat_prop / 100) %>% 
    select(-c(inc_median_total, inc_5, inc_10, inc_15, inc_20, inc_25, inc_30, 
              inc_35, inc_40, inc_45, inc_50, inc_60, inc_70, inc_80, inc_90, 
              inc_100, inc_high, inc_total, inc_limat_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  # Process identity
  var_list <- c("iden_imm_prop", "iden_imm_new_prop", 
                "iden_vm_prop", "iden_aboriginal_prop")
  
  data <- 
    data %>% 
    mutate(iden_imm_prop = imm / imm_total, 
           iden_imm_new_prop = imm_new / imm_total,
           iden_vm_prop = iden_vm / iden_vm_total,
           iden_aboriginal_prop = iden_aboriginal / iden_aboriginal_total) %>% 
    select(-c(imm, imm_new, imm_total, iden_vm, iden_vm_total, 
              iden_aboriginal, iden_aboriginal_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  # Process transport
  var_list <- c("trans_car_prop", "trans_walk_or_bike_prop", 
                "trans_transit_prop", "trans_t_15_prop", "trans_t_45_prop", 
                "trans_t_45_plus_prop")
  
  data <- 
    data %>% 
    mutate(trans_car_prop = (trans_driver + trans_passenger) / trans_total,
           trans_walk_or_bike_prop = (trans_walk + trans_bike) / trans_total,
           trans_transit_prop = trans_transit / trans_total,
           trans_t_15_prop = trans_t_15 / trans_t_total,
           trans_t_45_prop = (trans_t_30 + trans_t_45) / trans_t_total,
           trans_t_45_plus_prop = (trans_t_60 + trans_t_60_plus) / 
             trans_t_total) %>% 
    select(-c(trans_total, trans_driver, trans_passenger, trans_transit, 
              trans_walk, trans_bike, trans_t_total, trans_t_15,
              trans_t_30, trans_t_45, trans_t_60, trans_t_60_plus)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  # Process employment
  var_list <- c("emp_professional_prop", "emp_creative_prop")
  
  data <- 
    data %>% 
    mutate(emp_professional_prop = (emp_professional + emp_management) / emp_labour_total,
           emp_creative_prop = (emp_cultural + emp_arts) / emp_labour_total) %>% 
    select(-c(emp_professional, emp_management, emp_cultural, emp_labour_total,
              emp_arts)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  # Process family
  var_list <- c("family_children_prop", "family_one_person_prop")
  
  data <- 
    data %>% 
    mutate(family_children_prop = family_children / family_total,
           family_one_person_prop = family_one_person / family_total) %>% 
    select(-c(family_children, family_one_person, family_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  # Process language 
  var_list <- c("lang_french_only_prop", "lang_eng_only_prop", 
                "lang_french_eng_prop", "lang_no_official_prop")
  
  data <- 
    data %>% 
    mutate(lang_french_only_prop = lang_french_only / lang_total,
           lang_eng_only_prop = lang_eng_only / lang_total,
           lang_french_eng_prop = lang_eng_french / lang_total,
           lang_no_official_prop = lang_no_official / lang_total) %>% 
    select(-c(lang_french_only, lang_eng_only, lang_eng_french,
              lang_no_official, lang_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  # Process age
  var_list <- c("age_0_14_prop", "age_15_64_prop", "age_65_plus_prop")
  
  data <- 
    data %>% 
    mutate(age_0_14_prop = age_0_14 / age_total,
           age_15_64_prop = age_15_64 / age_total,
           age_65_plus_prop = age_65_plus / age_total) %>% 
    select(-c(age_0_14, age_15_64, age_65_plus, age_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  # Process education 
  var_list <- c("edu_bachelor_above_prop", "edu_no_degree_prop")
  
  data <- 
    data %>% 
    mutate(edu_bachelor_above_prop = edu_bachelor_above / edu_total,
           edu_no_degree_prop = edu_no_degree / edu_total) %>% 
    select(-c(edu_bachelor_above, edu_no_degree, edu_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  data <- st_set_agr(data, "constant") %>%
    mutate(across(where(is.numeric), ~replace(., is.nan(.), 0))) %>%
    mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA)))
  
  data
  
}


# Process data ------------------------------------------------------------

DA <- 
  DA %>% 
  left_join(census_geos$DA_census, by = "ID") %>% 
  process_census_data()

CT <- 
  CT %>% 
  left_join(census_geos$CT_census, by = "ID") %>% 
  process_census_data()

CSD <- 
  borough %>% 
  left_join(select(census_geos$CSD_census, -name), by = "ID") %>%
  process_census_data()


# Process boroughs --------------------------------------------------------

avg_list <- str_subset(names(census_geos$DA_census), "avg|median|prop") %>% 
  str_subset("total", negate = TRUE)

# Identify variables to be aggregated
agg_list <-
  setdiff(names(census_geos$DA_census), c("ID", "name", "CTUID", "CSDUID", "geometry", 
                                          "area")) %>% 
  setdiff(avg_list)

borough <-
  DA %>% 
  st_drop_geometry() %>% 
  select(ID, CSDUID) %>% 
  filter(str_starts(CSDUID, "2466023")) %>% 
  left_join(census_geos$DA_census, by = "ID") %>% 
  group_by(ID = CSDUID) %>% 
  summarize(
    housing_rent_avg_dollar = 
      weighted.mean(housing_rent_avg_dollar, rent_avg_total, na.rm = TRUE),
    housing_value_avg_dollar = 
      weighted.mean(housing_value_avg_dollar, value_avg_total, na.rm = TRUE),
    inc_median_dollar = weighted.mean(inc_median_dollar, inc_median_total, 
                                      na.rm = TRUE),
    housing_stress_renter_prop = 
      weighted.mean(housing_stress_renter_prop, rent_avg_total, na.rm = TRUE),
    housing_stress_owner_prop = 
      weighted.mean(housing_stress_owner_prop, value_=avg_total, na.rm = TRUE),
    inc_limat_prop = weighted.mean(inc_limat_prop, inc_limat_total, 
                                   na.rm = TRUE),
    across(all_of(agg_list), sum, na.rm = TRUE), 
    .groups = "drop") %>% 
  inner_join(borough, ., by = "ID") %>% 
  relocate(geometry, .after = last_col())

borough <- process_census_data(borough)

# Rbind CSD and borough_data
borough <-
  bind_rows(filter(CSD, !str_starts(ID, "2466023")), borough) %>%
  arrange(ID) %>% 
  st_set_agr("constant")


# Process grid ------------------------------------------------------------

DA_data <-
  DA %>% 
  select(ID) %>% 
  inner_join(census_geos$DA_census, by = "ID") %>% 
  st_transform(32618) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_set_agr("constant")

# Identify variables to be averaged
avg_list <- str_subset(names(DA_data), "avg|median") %>% 
  str_subset("total", negate = TRUE)

# Identify variables to be aggregated
agg_list <-
  setdiff(names(DA_data), c("ID", "name", "CTUID", "CSDUID", "geometry", 
                            "area")) %>% 
  setdiff(avg_list)

grid_census <-
  grid %>% 
  select(ID) %>% 
  st_transform(32618) %>% 
  st_set_agr("constant") %>% 
  st_intersection(DA_data) %>% 
  mutate(area_prop = st_area(geometry) / area) %>% 
  mutate(across(all_of(agg_list), ~{.x * units::drop_units(area_prop)})) %>% 
  select(-ID.1, -area, -area_prop) %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarize(
    housing_value_avg_dollar = 
      weighted.mean(housing_value_avg_dollar, value_avg_total, na.rm = TRUE),
    housing_rent_avg_dollar = 
      weighted.mean(housing_rent_avg_dollar, rent_avg_total, na.rm = TRUE),
    inc_median_dollar = 
      weighted.mean(inc_median_dollar, inc_median_total, na.rm = TRUE),
    housing_stress_renter_prop = weighted.mean(housing_stress_renter_prop, rent_avg_total, 
                                               na.rm = TRUE),
    housing_stress_owner_prop = weighted.mean(housing_stress_owner_prop, value_avg_total, 
                                              na.rm = TRUE),
    inc_limat_prop = weighted.mean(inc_limat_prop, inc_limat_total, 
                                   na.rm = TRUE),
    across(all_of(agg_list), sum, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.nan(.), NA)))

grid <- 
  grid %>% 
  left_join(grid_census, by = "ID") %>% 
  process_census_data()

rm(census_geos, CSD, DA_data, grid_census, agg_list,
   avg_list, process_census_data, year_census)