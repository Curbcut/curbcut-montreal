### HOUSING MODULE GLOBALS #####################################################

# Time slider values
housing_slider <- list(
  min = as.numeric(min_census_year),
  max = as.numeric(current_census),
  interval = 5,
  init = as.numeric(current_census))

# Dropdown menu
var_list_housing_left <- 
  list("Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_pct",
         "Average rent ($)" = "housing_rent_avg_dollar",
         "Average property value ($)" = "housing_value_avg_dollar",
         "Unaffordable housing (%)" = "housing_unafford_pct",
         "Unsuitable housing (%)" = "housing_unsuit_pct",
         "Housing requiring major repairs (%)" = "housing_repairs_pct",
         "Owner housing stress (%)" = "housing_stress_owner_pct",
         "Renter housing stress (%)" = "housing_stress_renter_pct"))