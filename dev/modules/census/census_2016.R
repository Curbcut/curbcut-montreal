#### Census data setup #########################################################

# This script relies on objects created in dev/build_geometries.R and
# dev/modules/census/build_census.R

# Setting variables -------------------------------------------------------

year_census <- "2016"

# Topic vectors -----------------------------------------------------------

census_housing <- c(
  tenure_households = "v_CA16_4836",
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

census_income <- c(
  inc_median_total = "v_CA16_2396",
  inc_median_dollar = "v_CA16_2397",
  inc_total = "v_CA16_2405",
  inc_5  = "v_CA16_2406", 
  inc_10 = "v_CA16_2407",
  inc_15 = "v_CA16_2408",
  inc_20 = "v_CA16_2409",
  inc_25 = "v_CA16_2410",
  inc_30 = "v_CA16_2411",
  inc_35 = "v_CA16_2412",
  inc_40 = "v_CA16_2413",
  inc_45 = "v_CA16_2414",
  inc_50 = "v_CA16_2415",
  inc_60 = "v_CA16_2416",
  inc_70 = "v_CA16_2417",
  inc_80 = "v_CA16_2418",
  inc_90 = "v_CA16_2419",
  inc_100 = "v_CA16_2420",
  inc_high = "v_CA16_2421",
  inc_limat_prop = "v_CA16_2540",
  inc_limat_total = "v_CA16_2510")

census_identity <- c(
  imm_total = "v_CA16_3405",
  imm = "v_CA16_3411",
  imm_new = "v_CA16_3432",
  iden_vm_total = "v_CA16_3954",
  iden_vm = "v_CA16_3957",
  iden_aboriginal = "v_CA16_3855",
  iden_aboriginal_total = "v_CA16_3852")

census_transport <- c(
  trans_total = "v_CA16_5792",
  trans_driver = "v_CA16_5795",
  trans_passenger = "v_CA16_5798",
  trans_transit = "v_CA16_5801",
  trans_walk = "v_CA16_5804",
  trans_bike = "v_CA16_5807",
  trans_t_total = "v_CA16_5813",
  trans_t_15 = "v_CA16_5816",
  trans_t_30 = "v_CA16_5819",
  trans_t_45 = "v_CA16_5822",
  trans_t_60 = "v_CA16_5825",
  trans_t_60_plus = "v_CA16_5828")

census_employment <- c(
  emp_labour_total = "v_CA16_5693",
  emp_professional = "v_CA16_5735",
  emp_management = "v_CA16_5738",
  emp_cultural = "v_CA16_5726",
  emp_arts = "v_CA16_5750")

census_family <- c(
  family_total = "v_CA16_504",
  family_children =  "v_CA16_507",
  family_one_person = "v_CA16_510")

census_language <- c(
  lang_french_only = "v_CA16_518",
  lang_eng_only = "v_CA16_515",
  lang_eng_french = "v_CA16_521",
  lang_no_official = "v_CA16_524",
  lang_total = "v_CA16_512")

census_age <- c(
  age_0_14 = "v_CA16_4",
  age_15_64 = "v_CA16_61",
  age_65_plus = "v_CA16_244",
  age_total = "v_CA16_1")

census_education <- c(
  edu_total = "v_CA16_5051",
  edu_bachelor_above = "v_CA16_5078",
  edu_no_degree = "v_CA16_5054")

# Download data -----------------------------------------------------------

source("dev/modules/census/census_retrieval_functions.R")

census_geos <- census_retrieval("CA16", added_var_group = 
                                  c(census_age, census_family, census_language))

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
  # Process income
  var_list <- c(var_list, "inc_median_dollar", "inc_50_prop", "inc_100_prop", 
                "inc_high_prop", "inc_limat_prop")
  # Process identity
  var_list <- c(var_list, "iden_imm_prop", "iden_imm_new_prop", 
                "iden_vm_prop", "iden_aboriginal_prop")
  # Process transport
  var_list <- c(var_list, "trans_car_prop", "trans_walk_or_bike_prop", 
                "trans_transit_prop", "trans_t_15_prop", "trans_t_45_prop", 
                "trans_t_45_plus_prop")
  # Process employment
  var_list <- c(var_list, "emp_professional_prop", "emp_creative_prop")
  # Process family
  var_list <- c(var_list, "family_children_prop", "family_one_person_prop")
  # Process language 
  var_list <- c(var_list, "lang_french_only_prop", "lang_eng_only_prop", 
                "lang_french_eng_prop", "lang_no_official_prop")
  # Process age
  var_list <- c(var_list, "age_0_14_prop", "age_15_64_prop", "age_65_plus_prop")
  # Process education 
  var_list <- c(var_list, "edu_bachelor_above_prop", "edu_no_degree_prop")
  
  
  data <- 
    data %>% 
    # Process housing
    mutate(housing_tenant_prop = renter / tenure_households,
           housing_unafford_prop = housing_unafford / housing_unafford_total,
           housing_unsuit_prop = housing_unsuit / housing_unsuit_total,
           housing_repairs_prop = major_repairs / repairs_total,
           housing_stress_renter_prop = housing_stress_renter_prop / 100,
           housing_stress_owner_prop = housing_stress_owner_prop / 100,
           housing_mobility_one_prop = housing_mobility_one / 
             housing_mobility_one_total,
           housing_mobility_five_prop = housing_mobility_five / 
             housing_mobility_five_total) %>% 
    select(-c(renter, tenure_households, housing_unafford, 
              housing_unafford_total, housing_unsuit, housing_unsuit_total,
              rent_avg_total, value_avg_total, major_repairs, repairs_total,
              housing_mobility_one, housing_mobility_one_total, 
              housing_mobility_five,
              housing_mobility_five_total)) %>% 
    # Process income
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
    # Process identity
    mutate(iden_imm_prop = imm / imm_total, 
           iden_imm_new_prop = imm_new / imm_total,
           iden_vm_prop = iden_vm / iden_vm_total,
           iden_aboriginal_prop = iden_aboriginal / iden_aboriginal_total) %>% 
    select(-c(imm, imm_new, imm_total, iden_vm, iden_vm_total, 
              iden_aboriginal, iden_aboriginal_total)) %>% 
    # Process transport
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
    # Process employment
    mutate(emp_professional_prop = (emp_professional + emp_management) / 
             emp_labour_total,
           emp_creative_prop = (emp_cultural + emp_arts) / emp_labour_total) %>% 
    select(-c(emp_professional, emp_management, emp_cultural, emp_labour_total,
              emp_arts)) %>% 
    # Process family
    mutate(family_children_prop = family_children / family_total,
           family_one_person_prop = family_one_person / family_total) %>% 
    select(-c(family_children, family_one_person, family_total)) %>% 
    # Process language 
    mutate(lang_french_only_prop = lang_french_only / lang_total,
           lang_eng_only_prop = lang_eng_only / lang_total,
           lang_french_eng_prop = lang_eng_french / lang_total,
           lang_no_official_prop = lang_no_official / lang_total) %>% 
    select(-c(lang_french_only, lang_eng_only, lang_eng_french,
              lang_no_official, lang_total)) %>% 
    # Process age
    mutate(age_0_14_prop = age_0_14 / age_total,
           age_15_64_prop = age_15_64 / age_total,
           age_65_plus_prop = age_65_plus / age_total) %>% 
    select(-c(age_0_14, age_15_64, age_65_plus, age_total)) %>% 
    # Process education 
    mutate(edu_bachelor_above_prop = edu_bachelor_above / edu_total,
           edu_no_degree_prop = edu_no_degree / edu_total) %>% 
    select(-c(edu_bachelor_above, edu_no_degree, edu_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry) %>% 
    rename_with(~paste0(.x, "_", year_census), 
                all_of(c(var_list, paste0(var_list, "_q3"))))
  
  data <- st_set_agr(data, "constant") %>%
    mutate(across(where(is.numeric), ~replace(., is.nan(.), NA))) %>%
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


# Process boroughs --------------------------------------------------------

avg_list <- str_subset(names(census_geos$DA_census), 
                       "avg|median|prop") %>% 
  str_subset("total|q3", negate = TRUE)

# Identify variables to be aggregated
agg_list <-
  setdiff(names(census_geos$DA_census), 
          c("ID", "name", "CTUID", "CSDUID", "geometry", "area")) %>% 
  setdiff(avg_list)

borough_raw <-
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
    across(all_of(agg_list), sum_na), 
    .groups = "drop") %>% 
  inner_join(select(borough, -c(name_2, population, households)), ., by = "ID") %>% 
  relocate(geometry, .after = last_col())

# Bind boroughs and CSDs before process_census_data
borough_CSD <- 
  bind_rows(filter(census_geos$CSD_census, !str_starts(ID, "2466023")), 
          borough_raw)

# Process data on both borough and CSD
borough <- 
  borough %>% 
  left_join(select(borough_CSD, -name, -geometry), by = "ID") %>%
  process_census_data() %>% 
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
    housing_stress_renter_prop = 
      weighted.mean(housing_stress_renter_prop, rent_avg_total, na.rm = TRUE),
    housing_stress_owner_prop = 
      weighted.mean(housing_stress_owner_prop, value_avg_total, na.rm = TRUE),
    inc_limat_prop = 
      weighted.mean(inc_limat_prop, inc_limat_total, na.rm = TRUE),
    across(all_of(agg_list), sum_na)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.nan(.), NA)))

grid <- 
  grid %>% 
  left_join(grid_census, by = "ID") %>% 
  process_census_data()


# Process building and street ---------------------------------------------

DA_data <- 
  DA |> 
  st_drop_geometry() |> 
  select(-c(name:buffer))

building <- 
  building |> 
  inner_join(DA_data, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col())

street <- 
  street |> 
  inner_join(DA_data, by = c("DAUID" = "ID")) |> 
  relocate(geometry, .after = last_col())


# Clean up ----------------------------------------------------------------

rm(borough_CSD, borough_raw, census_geos, DA_data, grid_census, agg_list, 
   avg_list, process_census_data, year_census)


