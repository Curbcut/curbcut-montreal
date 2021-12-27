### CLIMATE RISK MODULE GLOBALS ################################################

# Map token
token_climate_risk <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                             "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Dropdown menus
var_list_climate_risk <- 
  list("Destructive storms" = "climate_destructive_storms_ind",
       "Drought" = "climate_drought_ind",
       "Flood" = "climate_flood_ind",
       "Heat wave" = "climate_heat_wave_ind",
       "Heavy rain" = "climate_heavy_rain_ind")

var_list_scale <- 
  list("250-m grid" = "grid",
       "Boroughs and municipalities" = "borough",
       "Census tracts" = "CT",
       "Dissemination areas" = "DA")

# Initialize reactive values
climate_risk_zoom <- c("borough" = 0, "CT" = 10.5, "DA" = 12, "building" = 14)

rv_climate_risk <- reactiveValues(poly_selected = NA)

# Labels for boxplots
climate_legend <- c("1" = "Insignificant", "2" = "Minor", "3" = "Moderate", 
                    "4" = "Elevated", "5" = "Major")