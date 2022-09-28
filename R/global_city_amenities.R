### CITY AMENITIES GLOBALS #####################################################

# Main dropdown list
var_left_list_1_city_amenities <- 
  make_dropdown(only = list(theme = "City amenities"))

# Mode of transport
var_left_list_2_city_amenities <- 
  list("Mode of transport" = list(
    "15 minutes walking time" = "_walk_",
    "20 minutes cycling time" = "_bike_"
  ))