### GREEN SPACE GLOBALS #######################################################

# Initialize reactive values
rv_green_space <- reactiveValues(poly_selected = NA)

# Dropdown menu
green_space_groupings <- 
  list("Green space" = " ",
       "Per sq km" = "sqkm",
       "Per 1,000 residents" = "per1k")

green_space_type <- 
  list("Total" = "total",
       "Borough park" = "borough_park",
       "Large park" = "large_park",
       "Under validation" = "under_validation",
       "Road space" = "road_space",
       "Other green space" = "other")