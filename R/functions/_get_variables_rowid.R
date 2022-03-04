#### GET DROPDOWN LIST NUMBER #############################################

get_variables_rowid <- function(input) {
  
  if (!input %in% variables$var_code && !str_detect(input, "^\\d*$")) {
    
    return(input)
    
  } else {
    
    index <- 
      imap(variables$var_code, function(var_code, var_rowid) {
        out <- var_rowid
        names(out) <- var_code
        out
      }) |> unlist()
    
    if (str_detect(input[1], "^\\d*$")) return(names(index[index == input[1]]))
    if (!str_detect(input[1], "^\\d*$")) return(unname(index[names(index) == input[1]]))
    
  }
}