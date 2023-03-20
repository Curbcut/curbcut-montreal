variables <- qs::qread("data/variables.qs")
variables_var_left <- variables[variables$source == "Canadian census" &
                                  variables$theme == "Housing" &
                                  !is.na(variables$parent_vec), ]

id <- "housing"
var_left <- variables_var_left$var_code
var_right <- variables$var_code[variables$source == "Canadian census"  &
                                  variables$theme != "Housing" &
                                  !is.na(variables$parent_vec)]
time <- unlist(variables_var_left$dates) |> unique()
