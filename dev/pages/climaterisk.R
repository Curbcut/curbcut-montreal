# variables <- qs::qread("data/variables.qs")
# variables_var_left <- variables[grepl("^climate_", variables$var_code), ]
# 
# id <- "climaterisk"
# var_left <- variables_var_left$var_code
# var_right <- variables$var_code[variables$source == "Canadian census" &
#                                   !is.na(variables$parent_vec)]
# time <- 2017