variables <- qs::qread("data/variables.qs")
variables_var_left <- variables[grepl("^access_", variables$var_code), ]
variables_var_left <- variables_var_left[grepl("_30_", variables_var_left$var_code), ]

id <- "access"
var_left <- variables_var_left$var_code
var_right <- variables$var_code[variables$source == "Canadian census"]
group_name_label <- "Access"
time <- c("")