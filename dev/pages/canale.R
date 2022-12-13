variables <- qs::qread("data/variables.qs")

id <- "canale"
var_left <- "canale"
var_right <- variables$var_code[variables$source == "Canadian census"]
time <- 2016