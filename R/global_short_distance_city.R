### CITY AMENITIES GLOBALS #####################################################

# Main dropdown list
var_left_list_1_city_amenities <- 
  make_dropdown(only = list(theme = "City amenities")) |> 
  lapply(\(x) lapply(x, \(y) gsub("_bike", "", y)))

# Mode of transport
var_left_list_2_city_amenities <- 
  list("Mode of transport" = list(
    "15-minute walking time" = "walk",
    "20-minute cycling time" = "bike"
  ))