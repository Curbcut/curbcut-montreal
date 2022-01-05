### CRASH MODULE GLOBALS #######################################################

# Map token
map_zoom_crash_levels = c("borough" = 0, "CT" = 10.5, "DA" = 12, "street" = 14)

# Time slider values
crash_slider <- list(
  min = lubridate::year(min(crash$date)),
  max = lubridate::year(max(crash$date)),
  interval = 1,
  init = lubridate::year(max(crash$date)))

# Dropdown menu
var_list_left_crash_1 <- 
  list("Count" = "count",
       "Per sq km" = "sqkm",
       "Per 1,000 residents" = "per1k")

var_list_left_crash_2 <- 
  list("Total" = "total",
       "Pedestrian" = "ped",
       "Cyclist" = "cyc",
       "Other" = "other")

# Mapdeck point cloud legend
crash_legend_en <- 
  mapdeck_legend(
    legend_element(
      variables = c("Pedestrian", "Cyclist", "Other", "Unknown"),
      colours = c("#91BD9AEE","#6C83B5EE", "#F39D60EE", "#E8E8E8EE"),
      colour_type = "fill",
      variable_type = "category",
      title = "Crash type")
  )

crash_legend_fr <- 
  mapdeck_legend(
    legend_element(
      variables = c("Piéton", "Cycliste", "Autre", "Inconnu"),
      colours = c("#91BD9AEE","#6C83B5EE", "#F39D60EE", "#E8E8E8EE"),
      colour_type = "fill",
      variable_type = "category",
      title = "Type d'accident")
  )
