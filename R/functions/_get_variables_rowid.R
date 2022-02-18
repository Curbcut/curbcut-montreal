#### GET DROPDOWN LIST NUMBER #############################################

get_variables_rowid <- function(input) {
  
  if (input == " ") return(" ")

  index <- 
    imap(variables$var_code, function(var_code, var_rowid) {
      out <- var_rowid
      names(out) <- var_code
      out
    }) |> unlist()
  
  if (str_detect(input, "^\\d*$")) return(names(index[index == input]))
  if (!str_detect(input, "^\\d*$")) return(unname(index[names(index) == input]))
  
}
