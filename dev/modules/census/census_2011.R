#### Census data setup #########################################################

# This script relies on objects created in dev/build_geometries.R and
# dev/modules/census/build_census.R

# Setting variables -------------------------------------------------------

year_census <- "2011"

# 2011 Topic vectors -----------------------------------------------------------

census_housing <- c(
  tenure_households = "v_CA11N_2252", #NHS
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

census_income <- c(
  inc_median_total = "v_CA11N_2561", #NHS
  inc_median_dollar = "v_CA11N_2562", #NHS
  inc_total = "v_CA11N_2533", #NHS
  inc_5  = "v_CA11N_2534", #NHS
  inc_10 = "v_CA11N_2535", #NHS
  inc_15 = "v_CA11N_2536", #NHS
  inc_20 = "v_CA11N_2537", #NHS
  inc_30 = "v_CA11N_2538", #NHS
  inc_40 = "v_CA11N_2539", #NHS
  inc_50 = "v_CA11N_2540", #NHS
  inc_60 = "v_CA11N_2541", #NHS
  inc_80 = "v_CA11N_2542", #NHS
  inc_100 = "v_CA11N_2543", #NHS
  inc_125 = "v_CA11N_2544", #NHS
  inc_150 = "v_CA11N_2545", #NHS
  inc_high = "v_CA11N_2546", #NHS
  inc_low_income_prop = "v_CA11N_2606", #NHS
  inc_low_income_total = "v_CA11N_2576") #NHS

census_identity <- c(
  imm_total = "v_CA11N_16", #NHS
  imm = "v_CA11N_22", #NHS
  imm_new = "v_CA11N_43", #NHS
  iden_vm_total = "v_CA11N_457", #NHS
  iden_vm = "v_CA11N_460",
  iden_aboriginal = "v_CA11N_1354",
  iden_aboriginal_total = "v_CA11N_1351") #NHS

census_transport <- c(
  trans_total = "v_CA11N_2191", #NHS
  trans_driver = "v_CA11N_2194", #NHS
  trans_passenger = "v_CA11N_2197", #NHS
  trans_transit = "v_CA11N_2200", #NHS
  trans_walk = "v_CA11N_2203", #NHS
  trans_bike = "v_CA11N_2206") #NHS

census_employment <- c(
  emp_labour_total = "v_CA11N_2065",
  emp_professional = "v_CA11N_2107",
  emp_management = "v_CA11N_2110",
  emp_cultural = "v_CA11N_2098",
  emp_arts = "v_CA11N_2122")

census_education <- c(
  edu_total = "v_CA11N_1771",
  edu_bachelor_above = "v_CA11N_1792",
  edu_no_degree = "v_CA11N_1774")


# Download data -----------------------------------------------------------

census_geos <- census_retrieval("CA11")

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
    filter(units::drop_units(area_prop) > 0.02) %>% 
    group_by(ID.1) %>% 
    filter(sum(units::drop_units(area_prop)) >= 0.5) %>% 
    ungroup() %>% 
    select(-ID.1, -area, -area_prop) %>% 
    st_drop_geometry() %>% 
    group_by(ID) %>%
    summarize(
      housing_rent_avg_dollar = weighted.mean(housing_rent_avg_dollar, rent_avg_total, na.rm = TRUE),
      housing_value_avg_dollar = weighted.mean(housing_value_avg_dollar, value_avg_total, na.rm = TRUE),
      inc_median_dollar = weighted.mean(inc_median_dollar, inc_median_total, na.rm = TRUE),
      housing_stress_renter_prop = weighted.mean(housing_stress_renter_prop, rent_avg_total, na.rm = TRUE),
      housing_stress_owner_prop = weighted.mean(housing_stress_owner_prop, value_avg_total, na.rm = TRUE),
      inc_low_income_prop = weighted.mean(inc_low_income_prop, inc_low_income_total, na.rm = TRUE),
      across(all_of(agg_list), sum_na)) %>%
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
  filter(units::drop_units(area_prop) > 0.02) %>% 
  select(-ID, -area, -area_prop) %>% 
  st_drop_geometry() %>% 
  group_by(ID = CSDUID) %>%
  summarize(
    housing_rent_avg_dollar = weighted.mean(housing_rent_avg_dollar, rent_avg_total, na.rm = TRUE),
    housing_value_avg_dollar = weighted.mean(housing_value_avg_dollar, value_avg_total, na.rm = TRUE),
    inc_median_dollar = weighted.mean(inc_median_dollar, inc_median_total, na.rm = TRUE),
    housing_stress_renter_prop = weighted.mean(housing_stress_renter_prop, rent_avg_total, na.rm = TRUE),
    housing_stress_owner_prop = weighted.mean(housing_stress_owner_prop, value_avg_total, na.rm = TRUE),
    inc_low_income_prop = weighted.mean(inc_low_income_prop, inc_low_income_total, na.rm = TRUE),
    across(all_of(agg_list), sum_na)) %>%
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
                "housing_value_avg_dollar", "housing_repairs_prop", "housing_unsuit_prop",
                "housing_stress_renter_prop", "housing_stress_owner_prop",
                "housing_mobility_one_prop", "housing_mobility_five_prop")
  
  # income 
  var_list <- c(var_list, "inc_median_dollar", "inc_50_prop", "inc_100_prop", 
                "inc_high_prop", "inc_low_income_prop")
  
  # immigration
  var_list <- c(var_list, "iden_imm_prop", "iden_imm_new_prop", "iden_vm_prop", "iden_aboriginal_prop")
  
  # transport
  var_list <- c(var_list, "trans_car_prop", "trans_walk_or_bike_prop", 
                "trans_transit_prop")
  
  # employment
  var_list <- c(var_list, "emp_professional_prop", "emp_creative_prop")
  
  # education 
  var_list <- c(var_list, "edu_bachelor_above_prop", "edu_no_degree_prop")
  
  data <- 
    data %>% 
    # process housing
    mutate(housing_tenant_prop = renter / tenure_households,
           housing_unsuit_prop = housing_unsuit / housing_unsuit_total,
           housing_repairs_prop = major_repairs / repairs_total,
           housing_stress_renter_prop = housing_stress_renter_prop / 100,
           housing_stress_owner_prop = housing_stress_owner_prop / 100,
           housing_mobility_one_prop = housing_mobility_one / housing_mobility_one_total,
           housing_mobility_five_prop = housing_mobility_five / housing_mobility_five_total) %>% 
    select(-c(renter, tenure_households, value_avg_total, rent_avg_total,
              major_repairs, repairs_total, housing_unsuit, housing_unsuit_total,
              housing_mobility_one, housing_mobility_one_total, housing_mobility_five,
              housing_mobility_five_total)) %>% 
    # process income
    mutate(inc_50_prop = (inc_5 + inc_10 + inc_15 + inc_20 + inc_30 +
                            inc_40 + inc_50) / inc_total,
           inc_100_prop = (inc_60 + inc_80 + inc_100) / inc_total,
           inc_high_prop = (inc_125 + inc_150 + inc_high) / inc_total,
           inc_low_income_prop = inc_low_income_prop / 100) %>% 
    select(-c(inc_median_total, inc_5, inc_10, inc_15, inc_20, 
              inc_30, inc_40, inc_50, inc_60, inc_80, inc_100, 
              inc_125, inc_150, inc_high, inc_total, inc_low_income_total)) %>% 
    # process immigration/identity
    mutate(iden_imm_prop = imm / imm_total, 
           iden_imm_new_prop = imm_new / imm_total,
           iden_vm_prop = iden_vm / iden_vm_total,
           iden_aboriginal_prop = iden_aboriginal / iden_aboriginal_total) %>% 
    select(-c(imm, imm_new, imm_total, iden_vm, iden_vm_total,
              iden_aboriginal, iden_aboriginal_total)) %>% 
    # process transport
    mutate(trans_car_prop = (trans_driver + trans_passenger) / trans_total,
           trans_walk_or_bike_prop = (trans_walk + trans_bike) / trans_total,
           trans_transit_prop = trans_transit / trans_total) %>% 
    select(-c(trans_total, trans_driver, trans_passenger, 
              trans_transit, trans_walk, trans_bike)) %>% 
    # process employment
    mutate(emp_professional_prop = (emp_professional + emp_management) / emp_labour_total,
           emp_creative_prop = (emp_cultural + emp_arts) / emp_labour_total) %>% 
    select(-c(emp_professional, emp_management, emp_cultural, emp_labour_total,
              emp_arts)) %>% 
    # process education
    mutate(edu_bachelor_above_prop = edu_bachelor_above / edu_total,
           edu_no_degree_prop = edu_no_degree / edu_total) %>% 
    select(-c(edu_bachelor_above, edu_no_degree, edu_total)) %>% 
    # other manipulation
    mutate(across(contains("dollar"), ~ .x * 1.068)) %>% 
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


# Drop a variable if it's NAs at all scales -------------------------------

var_to_drop <- 
  c(DA_census %>% 
      select_if(~all(is.na(.))) %>% 
      names(),
    CT_census %>% 
      select_if(~all(is.na(.))) %>% 
      names(),
    borough_census %>% 
      select_if(~all(is.na(.))) %>% 
      names()) %>% tibble(var = .) %>% 
  mutate(var = str_remove(var, "_\\d{4}$")) %>% 
  count(var) %>% 
  filter(n == 3) %>% 
  pull(var)

DA_census <- 
  DA_census %>% select(!starts_with(var_to_drop))

CT_census <- 
  CT_census %>% select(!starts_with(var_to_drop))

borough_census <- 
  borough_census %>% select(!starts_with(var_to_drop))

grid_census <- 
  grid_census %>% select(!starts_with(var_to_drop))


# Assign new variables to principal dfs -----------------------------------

DA <- left_join(DA, DA_census, by = "ID")
CT <- left_join(CT, CT_census, by = "ID")
borough <- left_join(borough, borough_census, by = "ID")
grid <- left_join(grid, grid_census, by = "ID")
building <- left_join(building, DA_census, by = c("DAUID" = "ID"))
street <- left_join(street, DA_census, by = c("DAUID" = "ID"))


# Cleanup -----------------------------------------------------------------

rm(census_geos, CSD_census, CT_census, DA_census, year_census, grid_census,
   DA_data, interpolate_census, process_census_data, borough_census, 
   DA_census_n, avg_list, agg_list, var_to_drop)
