#### DATA MODULE GLOBALS #######################################################

get_data_type <- function(data, var_left, var_right) {
  
  var_left <- var_left()
  var_right <- var_right()
  
  if (var_right == " " && length(var_left) == 1) return("uni_q5")
  
  return(NULL)
  
}