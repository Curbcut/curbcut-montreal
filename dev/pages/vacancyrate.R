variables <- qs::qread("data/variables.qs")
variables_var_left <- variables[variables$theme == "Vacancy rate" &
                                  !is.na(variables$parent_vec), ]

id <- "vacancyrate"
var_left <- variables_var_left$var_code
var_right <- variables$var_code[variables$source == "Canadian census"  &
                                  !is.na(variables$parent_vec)]
time <- unlist(variables_var_left$dates) |> unique()
no_autozoom <- TRUE
autovars <- all(!is.null(unlist(variables_var_left$group_diff)))