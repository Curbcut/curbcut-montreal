variables <- qs::qread("data/variables.qs")
variables_var_left <- variables[grepl("^access_", variables$var_code), ]

id <- "access"
var_left <- variables_var_left$var_code
var_right <- variables$var_code[variables$source == "Canadian census"]
