### COVID MODULE GLOBALS #################################################

# Map token
token_covid <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                      "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initial zoom
map_zoom_covid <- 11

# Initialize reactive values
rv_covid <- reactiveValues(path_selected = NA, point_selected = NA, 
                           zoom = "CSD")

# Dropdown menu
var_list_covid <- 
  list("May 2020" = "may_2020",
       "July 2020" = "july_2020",
       "October 2020" = "oct_2020")