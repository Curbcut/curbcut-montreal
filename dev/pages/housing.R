variables <- qs::qread("data/variables.qs")
variables_var_left <- variables[variables$source == "Canadian census" &
                                  variables$theme == "Housing", ]

id <- "housing"
var_left <- variables_var_left$var_code
var_right <- variables$var_code[variables$source == "Canadian census"  &
                                  variables$theme != "Housing"]
time <- unlist(variables_var_left$dates) |> unique()
