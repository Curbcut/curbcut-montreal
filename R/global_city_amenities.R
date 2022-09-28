### CITY AMENITIES GLOBALS #####################################################

# Main dropdown list
var_left_list_1_city_amenities <- 
  make_dropdown(only = list(theme = "City amenities")) |> 
  lapply(\(x) lapply(x, \(y) gsub("_bike_avg", "", y)))

# Mode of transport
var_left_list_2_city_amenities <- 
  list("Mode of transport" = list(
    "15 minutes walking time" = "walk",
    "20 minutes cycling time" = "bike"
  ))