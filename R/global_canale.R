### CANALE MODULE GLOBALS ######################################################

# Zoom levels
canale_zoom <- c("borough" = 0, "CT" = 10.5, "DA" = 12, "building" = 14)

# Initialize reactive values
rv_canale <- reactiveValues(poly_selected = NA)

# canale ind for current census
canale_ind <- paste0("canale_ind", "_", current_census)
