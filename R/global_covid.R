### COVID MODULE GLOBALS #################################################

# Map token
token_covid <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                      "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initial zoom
map_zoom_covid <- 11

# Initialize reactive values
rv_covid <- reactiveValues(path_selected = NA, point_selected = NA, 
                           zoom = "borough")

# Dropdown menu
var_list_covid <- 
  list("May 2020" = "may_2020",
       "July 2020" = "july_2020",
       "October 2020" = "oct_2020")

legend_covid <- 
  list("Circuit des voies actives et sécuritaires" = "#FF5733FF",
    "Circulation locale" = "#FFD733FF",
    "Corridor piéton élargi" = "#5F940EFF",
    "Corridor projecté" = "#10A9A7FF",
    "File d'attente encadrée" = "#2D80CAFF",
    "Rue familiale et active" = "#FF7C2DFF",
    "Rue fermée" = "#6F2094FF",
    "Rue partiellement fermée" = "#75BB79FF")