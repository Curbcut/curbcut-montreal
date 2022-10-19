### ACCESSIBILITY TO AMENITIES GLOBALS #########################################

var_left_list_1_amenities <-
  make_dropdown(only = NULL, 
                only_vars = c(variables$var_code[
                  grepl("^amenities_", variables$var_code)]))
