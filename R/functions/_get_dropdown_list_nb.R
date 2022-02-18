#### GET DROPDOWN LIST NUMBER #############################################

get_dropdown_list_nb <- function(input) {
  
  index <- 
    imap(unname(make_dropdown()), function(listed_vars, listed_nb) {
      imap(unname(listed_vars), ~{
        out <- paste(listed_nb, .y, sep = "_")
        names(out) <- .x
        out
      })
    }) |> unlist()
  
  if (str_detect(input, "^\\d")) return(names(index[index == input]))
  if (!str_detect(input, "^\\d")) return(unname(index[names(index) == input]))
  
}
get_dropdown_list_nb("inc_median_dollar")
