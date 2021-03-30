### CLIMATE RISK MODULE GLOBALS ################################################

# Map token
token_climate_risk <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                             "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Dropdown menus
var_list_climate_risk <- 
  list("Destructive storms" = "destructive_storms_ind",
       "Drought" = "drought_ind",
       "Flood" = "flood_ind",
       "Heat wave" = "heat_wave_ind",
       "Heavy rain" = "heavy_rain_ind")

var_list_scale <- 
  list("250-m grid" = "grid",
       "Boroughs and municipalities" = "borough",
       "Census tracts" = "CT",
       "Dissemination areas" = "DA")

# Initialize reactive values
rv_climate_risk <- reactiveValues(zoom = "borough", poly_selected = NA)

# Labels for boxplots
climate_legend <- c("0" = "Insignificant", "1" = "Minor", "2" = "Moderate", 
                    "3" = "Elevated", "4" = "Major")