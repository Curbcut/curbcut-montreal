#### GET DROPDOWN LIST NUMBER ##################################################

get_variables_rowid <- function(input) {
  
  if (!input %in% variables$var_code && !str_detect(input, "^\\d*$")) {
    
    input
    
  } else {
    
    index <- setNames(seq_along(variables$var_code), variables$var_code)
    
    if (str_detect(input[1], "^\\d*$")) {
      names(index[index == input[1]])
    } else unname(index[names(index) == input[1]])
    
  }
}
