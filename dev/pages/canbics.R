variables <- qs::qread("data/variables.qs")

id <- "canbics"
var_left <- "canbics"
var_right <- variables$var_code[variables$source == "Canadian census" &
                                  !is.na(variables$parent_vec)]
time <- 2021