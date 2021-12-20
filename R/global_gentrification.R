### GI MODULE GLOBALS ######################################################

# Map token
token_gentrification <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                       "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
gentrification_zoom <- c("borough" = 0, "CT" = 10.5, "DA" = 12, "building" = 14)

rv_gentrification <- reactiveValues(poly_selected = NA)

# Time slider values
gentrification_slider <- list(
  min = 1996,
  max = 2016,
  interval = 5,
  init = c(2006,2016))

# Left-side dropdown menu
var_list_left_gentrification <- 
  list(#"----" = " ", 
    "Bachelor and above (%)" = "edu_bachelor_above_prop", 
    "Median household income ($)" = "inc_median_dollar", 
    "Average property value ($)" = "housing_value_avg_dollar",
    "Average rent ($)" = "housing_rent_avg_dollar", 
    "Tenant-occupied (%)" = "housing_tenant_prop", 
    "Visible minorities (%)" = "iden_vm_prop")
