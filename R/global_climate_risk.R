### CLIMATE RISK MODULE GLOBALS ################################################

# Dropdown menus
var_list_climate_risk <- 
  list("Destructive storms" = "climate_destructive_storms_ind",
       "Drought" = "climate_drought_ind",
       "Flood" = "climate_flood_ind",
       "Heat wave" = "climate_heat_wave_ind",
       "Heavy rain" = "climate_heavy_rain_ind")

# Labels for boxplots
climate_legend <- c("0" = "No risk", "1" = "Insignificant", 
                    "2" = "Minor", "3" = "Moderate", "4" = "Elevated", 
                    "5" = "Major")