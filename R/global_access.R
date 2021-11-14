### ACCESS MODULE GLOBALS ######################################################

# Map token
token_access <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                       "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
rv_access <- reactiveValues(poly_selected = NA)

# Dropdown menu
var_list_left_access_1 <- 
  list("All jobs" = "access_jobs_total",
       "Low-skill jobs" = "access_jobs_low",
       "High-skill jobs" = "access_jobs_high",
       "Jobs < $30,000 annually" = "access_jobs_30k",
       "Schools" = "access_schools",
       "Healthcare facilities" = "access_healthcare")

var_list_left_access_2 <- 
  list("Weekday peak" = "pwd",
       "Weekday off-peak" = "opwd",
       "Weekday night" = "nwd",
       "Weekend peak" = "pwe",
       "Weekend off-peak" = "opwe",
       "Weekend night" = "nwe")

# Dropdown menu
var_list_right_access <- 
  list("----" = " ", 
       "Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_prop",
         "Average rent ($)" = "housing_rent_avg_dollar",
         "Average property value ($)" = "housing_prop_value_avg_dollar",
         "Unaffordable housing (%)" = "housing_unafford_prop",
         "Unsuitable housing (%)" = "housing_unsuit_prop"),
       "Income" = list(
         "Median household income ($)" = "inc_median_dollar",
         "Income under $50k (%)" = "inc_50_prop",
         "Income between $50k-$100k (%)" = "inc_100_prop",
         "Income above $100k (%)" = "inc_high_prop"),
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
         "More than 45 minutes to work (%)" = "trans_t_45_plus_prop"))

var_list_right_access[-1] <-
  var_list_right_access[-1] %>%
  purrr::modify_depth(2, paste0, "_", current_census)

access_colour <- colorRamp(colour_scale[1:3])

