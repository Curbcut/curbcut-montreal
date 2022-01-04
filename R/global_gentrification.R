### GENTRIFICATION MODULE GLOBALS #############################################

# Time slider values
gentrification_slider <- list(
  min = 1996,
  max = 2016,
  interval = 5,
  init = c(2006,2016))

# Left-side dropdown menu
var_list_left_gentrification <- 
  list("Bachelor and above (%)" = "edu_bachelor_above_pct", 
       "Median household income ($)" = "inc_median_dollar", 
       "Average property value ($)" = "housing_value_avg_dollar",
       "Average rent ($)" = "housing_rent_avg_dollar", 
       "Tenant-occupied (%)" = "housing_tenant_pct", 
       "Visible minorities (%)" = "iden_vm_pct")
