### DMTI MODULE GLOBALS ######################################################


# Map token
token_dmti <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                     "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")


# Initialize reactive values
rv_dmti <- reactiveValues(poly_selected = NA, zoom = "CSD")


# Time slider values
dmti_slider <- list(
  min = 2006,
  max = 2016,
  interval = 5,
  init = 2016)

# Dropdown menu 1
var_list_left_dmti_1 <- list("Food Amenities" = "food",
                             "City Features" = "city",
                             "Street Features" = "street",
                             "Accessibility to Healthcare" = "access",
                             "Exposure to Pollutants" = "exposure")

# Dropdown menu 2
var_list_left_dmti_2 <- list("Healthy" = "healthy")

var_list_left_dmti_2_food <-   
  list("Healthy" = "healthy",
       "Unhealthy" = "unhealthy",
       "Total" = "total")

var_list_left_dmti_2_city <-   
  list("Healthy" = "healthy",
       "Unhealthy" = "unhealthy",
       "Total" = "total")

var_list_left_dmti_2_street <-   
  list("Healthy" = "healthy",
       "Unhealthy" = "unhealthy",
       "Total" = "total")

var_list_left_dmti_2_access <- 
  list("Healthcare Facilities" = "healthcare")

var_list_left_dmti_2_exposure <- 
  list("Manufacturing" = "manufacturing")

# Dropdown menu 3
var_list_left_dmti_3 <- list("Count" = " ")

var_list_left_dmti_3_food <- 
  list("Count" = " ",
       "Percentage" = "prop",
       "Per 1000 residents" = "prop_pop")

var_list_left_dmti_3_city <- 
  list("Count" = " ",
       "Percentage" = "prop")

var_list_left_dmti_3_street <- 
  list("Count" = " ",
       "Percentage" = "prop")

var_list_left_dmti_3_access <- 
  list("Count" = " ",
       "Per 1000 residents" = "prop_pop")

var_list_left_dmti_3_exposure <-
  list("Count" = " ",
       "Per 1000 residents" = "prop_pop")


# Right dropdown menu
var_list_right_dmti <- 
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
       "Immigration" = list(
         "Immigrants (%)" =  "imm_prop",
         "New immigrants (%)" = "imm_new_prop"),
       "Transportation" = list(
         "Drive to work (%)" = "trans_car_prop",
         "Walk or cycle to work (%)" = "trans_walk_or_bike_prop",
         "Public transit to work (%)" = "trans_transit_prop",
         "15 minutes to work (%)" = "trans_t_15_prop",
         "15-45 minutes to work (%)" = "trans_t_45_prop",
         "More than 45 minutes to work (%)" = "trans_t_45_plus_prop"))
