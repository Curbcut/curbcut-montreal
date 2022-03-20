#### HOUSING MODULE GLOBALS ####################################################

vars_housing_left <- make_dropdown(include_only = "Housing")
vars_housing_right <- make_dropdown(exclude = "Housing")

vars_housing_left_dis <- unlist(vars_housing_left) %in% {
  v <- variables[variables$var_code %in% unlist(vars_housing_left),]
  v$var_code[!lengths(v$dates) == max(lengths(v$dates))]}

vars_housing_right_dis <- unlist(vars_housing_right) %in% {
  v <- variables[variables$var_code %in% unlist(vars_housing_right),]
  v$var_code[!lengths(v$dates) == max(lengths(v$dates))]}
