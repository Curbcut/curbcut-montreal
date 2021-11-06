#### Census data setup #########################################################

# This script relies on objects created in dev/build_geometries.R and
# dev/modules/census/build_census.R

# Setting variables -------------------------------------------------------

year_census <- "2006"

# 2006 Topic vectors -----------------------------------------------------------

census_housing <- c(
  tenure_households = "v_CA06_101", 
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

census_income <- c(
  inc_median_total = "v_CA06_1988", #also inc_total
  inc_median_dollar = "v_CA06_2000", 
  inc_10 = "v_CA06_1989",
  inc_20 = "v_CA06_1990", 
  inc_30 = "v_CA06_1991", 
  inc_40 = "v_CA06_1992", 
  inc_50 = "v_CA06_1993", 
  inc_60 = "v_CA06_1994", 
  inc_70 = "v_CA06_1995",
  inc_80 = "v_CA06_1996", 
  inc_90 = "v_CA06_1997",
  inc_100 = "v_CA06_1998", 
  inc_high = "v_CA06_1999", 
  inc_low_income_prop = "v_CA06_1981",
  inc_low_income_total = "v_CA06_1979")

census_identity <- c(
  imm_total = "v_CA06_474", 
  imm = "v_CA06_478", 
  imm_new = "v_CA06_553", 
  iden_vm_total = "v_CA06_1302",
  iden_vm = "v_CA06_1303",
  iden_aboriginal = "v_CA06_565",
  iden_aboriginal_total = "v_CA06_564") 

census_transport <- c(
  trans_total = "v_CA06_1100",
  trans_driver = "v_CA06_1101", 
  trans_passenger = "v_CA06_1102", 
  trans_transit = "v_CA06_1103", 
  trans_walk = "v_CA06_1104", 
  trans_bike = "v_CA06_1105")

census_employment <- c(
  emp_labour_total = "v_CA06_1007",
  emp_professional = "v_CA06_1021",
  emp_management = "v_CA06_1022",
  emp_cultural = "v_CA06_1018",
  emp_arts = "v_CA06_1026")

census_education <- c(
  edu_total_15_24 = "v_CA06_1234",
  edu_total_25_64 = "v_CA06_1248",
  edu_total_65_plus = "v_CA06_1262",
  edu_bachelor_above_15_24 = "v_CA06_1242",
  edu_bachelor_above_25_64 = "v_CA06_1256",
  edu_bachelor_above_65_plus = "v_CA06_1270",
  edu_no_degree_15_24 = "v_CA06_1235",
  edu_no_degree_25_64 = "v_CA06_1249",
  edu_no_degree_65_plus = "v_CA06_1263")


# Download data -----------------------------------------------------------

source("dev/modules/census/census_retrieval_functions.R")

census_geos <- census_retrieval("CA06")

rm(census_housing, census_identity, census_income, census_transport,
   census_employment, census_education, vars_to_remove, census_retrieval)


# Interpolate DA/CT/CSD geometries ----------------------------------------

interpolate_census <- function(new_data, principal_data) {
  
  # Get area for each geometry for upcoming interpolation function 
  new_data <-
    new_data %>% 
    st_transform(32618) %>% 
    mutate(area = st_area(geometry)) %>% 
    st_set_agr("constant")
  
  # Identify variables to be averaged
  avg_list <- str_subset(names(new_data), "avg|median|prop") %>% 
    str_subset("total", negate = TRUE)
  
  # Identify variables to be aggregated
  agg_list <-
    setdiff(names(new_data), c("ID", "name", "CTUID", "CSDUID", "geometry", 
                               "area")) %>% 
    setdiff(avg_list)
  
  new_data <-
    principal_data %>% 
    select(ID) %>% 
    st_transform(32618) %>% 
    st_set_agr("constant") %>% 
    st_intersection(., new_data) %>% 
    mutate(area_prop = st_area(geometry) / area) %>% 
    mutate(across(all_of(agg_list), ~{.x * units::drop_units(area_prop)})) %>% 
    group_by(ID) %>% 
    filter(sum(units::drop_units(area_prop)) >= 0.5) %>% 
    ungroup() %>% 
    select(-ID.1, -area, -area_prop) %>% 
    st_drop_geometry() %>% 
    group_by(ID) %>%
    summarize(
      housing_rent_avg_dollar = weighted.mean(housing_rent_avg_dollar, rent_avg_total, na.rm = TRUE),
      housing_value_avg_dollar = weighted.mean(housing_value_avg_dollar, value_avg_total, na.rm = TRUE),
      inc_median_dollar = weighted.mean(inc_median_dollar, inc_median_total, na.rm = TRUE),
      inc_low_income_prop = weighted.mean(inc_low_income_prop, inc_low_income_total, na.rm = TRUE),
      across(all_of(agg_list), sum, na.rm = TRUE)) %>% 
    mutate(across(where(is.numeric), ~replace(., is.nan(.), 0))) %>%
    mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) %>%
    mutate(across(all_of(agg_list), ~if_else(.x < 5, 0, .x)))
  
  new_data
  
}

DA_census <- interpolate_census(census_geos$DA_census, DA)
CT_census <- interpolate_census(census_geos$CT_census, CT)
CSD_census <- interpolate_census(census_geos$CSD_census, borough)


# Interpolate borough geometries ------------------------------------------

# Once DA interpolation has been done, I can look at boroughs using their geos

# Get area for DA geometry
DA_census_n <-
  DA_census %>% 
  left_join(select(DA, ID, CSDUID), by = "ID") %>% 
  st_as_sf() %>% 
  st_transform(32618) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_set_agr("constant")

avg_list <- str_subset(names(DA_census_n), "avg|median|prop") %>% 
  str_subset("total", negate = TRUE)

# Identify variables to be aggregated
agg_list <-
  setdiff(names(DA_census_n), c("ID", "name", "CTUID", "CSDUID", "geometry", 
                                "area")) %>% 
  setdiff(avg_list)

borough_census <- 
  borough %>% 
  select(ID) %>% 
  filter(str_starts(ID, "2466023")) %>% 
  st_transform(32618) %>% 
  st_set_agr("constant") %>% 
  st_filter(DA_census_n, .) %>% 
  mutate(area_prop = st_area(geometry) / area) %>% 
  mutate(across(all_of(agg_list), ~{.x * units::drop_units(area_prop)})) %>% 
  select(-ID, -area, -area_prop) %>% 
  st_drop_geometry() %>% 
  group_by(ID = CSDUID) %>%
  summarize(
    housing_rent_avg_dollar = weighted.mean(housing_rent_avg_dollar, rent_avg_total, na.rm = TRUE),
    housing_value_avg_dollar = weighted.mean(housing_value_avg_dollar, value_avg_total, na.rm = TRUE),
    inc_median_dollar = weighted.mean(inc_median_dollar, inc_median_total, na.rm = TRUE),
    inc_low_income_prop = weighted.mean(inc_low_income_prop, inc_low_income_total, na.rm = TRUE),
    across(all_of(agg_list), sum, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.nan(.), 0))) %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) %>%
  mutate(across(all_of(agg_list), ~if_else(.x < 5, 0, .x))) %>%
  filter(str_starts(ID, "2466023"))


# Interpolate grid geometries ---------------------------------------------

DA_data <- 
  DA %>% 
  select(ID) %>% 
  # Interpolation has already been done for DAs in DA_census, so I can left_join
  # to the principal DA geometries.
  left_join(DA_census, by = "ID") %>% 
  st_transform(32618) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_set_agr("constant")

grid_census <- interpolate_census(DA_data, grid)


# Helper function for processing data -------------------------------------

process_census_data <- function(data) {
  
  # housing
  var_list <- c("housing_tenant_prop", "housing_rent_avg_dollar", 
                "housing_value_avg_dollar", "housing_repairs_prop",
                "housing_stress_renter_prop", "housing_stress_owner_prop",
                "housing_mobility_one_prop", "housing_mobility_five_prop")
  # income
  var_list <- c(var_list, "inc_median_dollar", "inc_50_prop", "inc_100_prop", 
                "inc_high_prop", "inc_low_income_prop")
  # identity
  var_list <- c(var_list, "iden_imm_prop", "iden_imm_new_prop", "iden_vm_prop", 
                "iden_aboriginal_prop")
  # transport
  var_list <- c(var_list, "trans_car_prop", "trans_walk_or_bike_prop", 
                "trans_transit_prop")
  # employment
  var_list <- c(var_list, "emp_professional_prop", "emp_creative_prop")
  # education 
  var_list <- c(var_list, "edu_bachelor_above_prop", "edu_no_degree_prop")
  
  
  data <- 
    data %>% 
    # housing
    mutate(housing_tenant_prop = renter / tenure_households,
           housing_repairs_prop = major_repairs / repairs_total,
           housing_stress_renter_prop = housing_stress_renter / rent_avg_total,
           housing_stress_owner_prop = housing_stress_owner / value_avg_total,
           housing_mobility_one_prop = housing_mobility_one / housing_mobility_one_total,
           housing_mobility_five_prop = housing_mobility_five / housing_mobility_five_total) %>% 
    select(-c(renter, tenure_households, value_avg_total, rent_avg_total,
              major_repairs, repairs_total, housing_stress_renter, housing_stress_owner,
              housing_mobility_one, housing_mobility_one_total, housing_mobility_five,
              housing_mobility_five_total)) %>% 
    # income
    mutate(inc_50_prop = (inc_10 + inc_20 + inc_30 +
                            inc_40 + inc_50) / inc_median_total,
           inc_100_prop = (inc_60 + inc_70 + inc_80 + inc_90 + 
                             inc_100) / inc_median_total,
           inc_high_prop = inc_high / inc_median_total,
           inc_low_income_prop = inc_low_income_prop / 100) %>% 
    select(-c(inc_median_total, inc_10, inc_20, inc_30, inc_40, 
              inc_50, inc_60, inc_70, inc_80, inc_90, 
              inc_100, inc_high, inc_low_income_total)) %>% 
    # identity/immigration
    mutate(iden_imm_prop = imm / imm_total, 
           iden_imm_new_prop = imm_new / imm_total,
           iden_vm_prop = iden_vm / iden_vm_total,
           iden_aboriginal_prop = iden_aboriginal / iden_aboriginal_total) %>% 
    select(-c(imm, imm_new, imm_total, iden_vm, iden_vm_total,
              iden_aboriginal, iden_aboriginal_total)) %>%
    # transport
    mutate(trans_car_prop = (trans_driver + trans_passenger) / trans_total,
           trans_walk_or_bike_prop = (trans_walk + trans_bike) / trans_total,
           trans_transit_prop = trans_transit / trans_total) %>% 
    select(-c(trans_total, trans_driver, trans_passenger, 
              trans_transit, trans_walk, trans_bike)) %>% 
    # employment
    mutate(emp_professional_prop = (emp_professional + emp_management) / emp_labour_total,
           emp_creative_prop = (emp_cultural + emp_arts) / emp_labour_total) %>% 
    select(-c(emp_professional, emp_management, emp_cultural, emp_labour_total,
              emp_arts)) %>% 
    # education
    mutate(edu_bachelor_above_prop = (edu_bachelor_above_15_24 + edu_bachelor_above_25_64 + edu_bachelor_above_65_plus) / (edu_total_15_24 + edu_total_25_64 + edu_total_65_plus),
           edu_no_degree_prop = (edu_no_degree_15_24 + edu_no_degree_25_64 + edu_no_degree_65_plus) / (edu_total_15_24 + edu_total_25_64 + edu_total_65_plus)) %>% 
    select(-c(edu_total_15_24, edu_total_25_64, edu_total_65_plus, edu_bachelor_above_15_24,
              edu_bachelor_above_25_64, edu_bachelor_above_65_plus, edu_no_degree_15_24,
              edu_no_degree_25_64, edu_no_degree_65_plus)) %>% 
    # other processing
    mutate(across(contains("dollar"), ~ .x * 1.1741)) %>% 
    mutate(across(where(is.numeric), ~replace(., is.nan(.), NA))) %>% 
    mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA))) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3"))  %>% 
    rename_with(~paste0(.x, "_", year_census), all_of(c(var_list, paste0(var_list, "_q3"))))
  
  data
  
}

DA_census <- 
  DA_census %>% 
  process_census_data()

CT_census <- 
  CT_census %>% 
  process_census_data()

borough_census <- 
  CSD_census %>% 
  filter(!str_starts(ID, "2466023")) %>% 
  rbind(borough_census) %>% 
  process_census_data()

grid_census <- 
  grid_census %>% 
  process_census_data()


# Assign new variables to principal dfs -----------------------------------

DA <- left_join(DA, DA_census, by = "ID")
CT <- left_join(CT, CT_census, by = "ID")
borough <- left_join(borough, borough_census, by = "ID")
grid <- left_join(grid, grid_census, by = "ID")
building <- left_join(building, DA_census, by = c("DAUID" = "ID"))
street <- left_join(street, DA_census, by = c("DAUID" = "ID"))


# Cleanup -----------------------------------------------------------------

rm(census_geos, CSD_census, CT_census, DA_census, year_census, grid_census,
   DA_data, interpolate_census, process_census_data, borough_census, DA_census_n,
   avg_list, agg_list)
