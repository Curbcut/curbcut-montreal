### PEDESTRIAN MODULE GLOBALS ##################################################

# Map token
token_ped <- paste0("pk.eyJ1Ijoic2tvaG45MCIsImEiOiJja2JpNGZjMnUwYm9h",
                    "MnFwN3Q2bmV5c3prIn0.M-AJKxYD1ETFiBB6swQmJw")

# Initialize reactive values
rv_ped <- reactiveValues(zoom = "OUT", poly_selected = NA)

# Dropdown menus
var_list_ped <- list("Walkable Access to Key Amenities" = "agg_proximity_score",
                     "Net Median Income" = "net_median_income",
                     "Visible Minority Population Proportion" = "minority_percent", 
                     "Immigrant Population Proportion" = "immigrant_percent")

var_list_ped_slider <- list("Population density per square km" = 1, 
                            "Pedestrian social distancing capacity" = 2, 
                            "Work commutes by car (%)" = 3, 
                            "Trajet MTL 2016 data on pedestrian flows" = 4)
