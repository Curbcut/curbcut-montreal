#' @param x A one-row tibble

get_dyk_link_vars <- function(x) {
  
  # Case for no var_left
  if (x$module %in% c("canale")) vl <- NULL
  
  # TKTK other cases
  
  # Get var_right value
  vr <- if (length(x$variable[[1]]) == 1) " " else x$variable[[1]][2]
  
  return(list(vl, vr))
}