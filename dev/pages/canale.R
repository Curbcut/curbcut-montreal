variables <- qs::qread("data/variables.qs")

id <- "canale"
var_left <- "canale"
var_right <- variables$var_code[variables$source == "Canadian census" &
                                !is.na(variables$parent_vec)]
time <- variables$dates[variables$var_code == var_left][[1]]
