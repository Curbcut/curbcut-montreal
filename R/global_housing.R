#### HOUSING MODULE GLOBALS ####################################################

vars_housing_left <- make_dropdown(include_only = "Housing")
vars_housing_right <- make_dropdown(exclude = "Housing")

vars_housing_left_dis <- unlist(vars_housing_left) %in% {
  variables |> 
    filter(var_code %in% unlist(vars_housing_left)) |> 
    filter(!lengths(dates) == max(lengths(dates))) |> 
    pull(var_code)}

vars_housing_right_dis <- unlist(vars_housing_right) %in% {
  variables |> 
    filter(var_code %in% unlist(vars_housing_right)) |> 
    filter(!lengths(dates) == max(lengths(dates))) |> 
    pull(var_code)}
