### CANALE MODULE GLOBALS ######################################################

# Map token
token_housing <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                       "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
rv_housing <- reactiveValues(poly_selected = NA, zoom = "borough")

# Time slider values
housing_slider <- list(
  min = as.numeric(min_census_year),
  max = as.numeric(current_census),
  interval = 5,
  init = as.numeric(current_census))

# Dropdown menu
var_list_housing_left <- 
  list("Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_prop",
         "Average rent ($)" = "housing_rent_avg_dollar",
         "Average property value ($)" = "housing_value_avg_dollar",
         "Unaffordable housing (%)" = "housing_unafford_prop",
         "Unsuitable housing (%)" = "housing_unsuit_prop",
         "Housing requiring major repairs (%)" = "housing_repairs_prop",
         "Owner housing stress (%)" = "housing_stress_owner_prop",
         "Renter housing stress (%)" = "housing_stress_renter_prop"))

# When we need to disable values that aren't shared in every census
var_shared_housing_left <- 
  borough %>% 
  st_drop_geometry() %>% 
  select(contains(all_of(as.character(unlist(var_list_housing_left)))),
         -contains("_q3")) %>% 
  names() %>% 
  str_remove(., "_\\d{4}$") %>% 
  as_tibble() %>% 
  count(value) %>% 
  filter(n == max(n)) %>% 
  pull(value)

disabled_var_list_housing_left <- 
  !unlist(var_list_housing_left) %in% var_shared_housing_left

var_list_housing_right <-
  list("----" = " ", 
       "Income" = list(
         "Median household income ($)" = "inc_median_dollar",
         "Income under $50k (%)" = "inc_50_prop",
         "Income between $50k-$100k (%)" = "inc_100_prop",
         "Income above $100k (%)" = "inc_high_prop",
         "Prevalence of low income (after-tax) (%)" = "inc_limat_prop"),
       "Immigration and ethnicity" = list(
         "Immigrants (%)" =  "iden_imm_prop",
         "New immigrants (%)" = "iden_imm_new_prop",
         "Visible minorities (%)" = "iden_vm_prop"),
       "Transportation" = list(
         "Drive to work (%)" = "trans_car_prop",
         "Walk or cycle to work (%)" = "trans_walk_or_bike_prop",
         "Public transit to work (%)" = "trans_transit_prop",
         "15 minutes to work (%)" = "trans_t_15_prop",
         "15-45 minutes to work (%)" = "trans_t_45_prop",
         "More than 45 minutes to work (%)" = "trans_t_45_plus_prop")
       )

var_right_housing_shared <- 
  borough %>% 
  st_drop_geometry() %>% 
  select(contains(all_of(as.character(unlist(var_list_housing_right)))),
         -contains("_q3")) %>% 
  names() %>% 
  str_remove(., "_\\d{4}$") %>% 
  as_tibble() %>% 
  count(value) %>% 
  filter(n == max(n)) %>% 
  pull(value)

disabled_var_list_housing_right <- 
  (!unlist(var_list_housing_right) %in% var_right_housing_shared) %>% 
  replace(1, FALSE)
