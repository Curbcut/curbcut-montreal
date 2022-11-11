#### GET EXPLORE GRAPH AXIS LABELS #############################################

get_axis_labels <- function(r = r, graph_type, var_left, var_right) {
  
  ## Get basic axis labels -----------------------------------------------------
  
  var_left_title <- cc_t(r = r, variables$var_short[
    variables$var_code == unique(sub("_\\d{4}$", "", var_left))])

  var_right_title <- cc_t(r = r, variables$var_short[
    variables$var_code == unique(sub("_\\d{4}$", "", var_right))])
  
  
  ## Construct labs_xy based on graph_type -------------------------------------
  
  if (graph_type %in% c("hist", "bar")) labs_xy <- 
    list(labs(x = var_left_title, y = NULL))
  
  if (graph_type == "scatter") {
    
    date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
    date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
    
    if (!is.na(date_left))
      var_left_title <-
      paste0(var_left_title, " (", date_left, ")")
    
    if (!is.na(date_right))
      var_right_title <-
      paste0(var_right_title, " (", date_right, ")")
    
    labs_xy <-
      list(labs(x = var_right_title, y = var_left_title))
  }
  
  if (graph_type == "box") labs_xy <- 
    list(labs(x = var_left_title, y = var_right_title))
  
  if (graph_type %in% c("delta", "NAdelta")) labs_xy <- list(labs(
    x = paste0(var_left_title, " (", 
               str_extract(var_left, "(?<=_)\\d{4}$")[1], ")"),
    y = paste0(var_left_title, " (", 
               str_extract(var_left, "(?<=_)\\d{4}$")[2], ")")))
  
  if (graph_type %in% c("deltabivar", "NAdeltabivar")) {
    
    var_right_lab <- 
      if (length(unique(var_right)) == 2) {
        paste0(var_right_title, " (\u0394 ", 
               str_extract(var_right, "(?<=_)\\d{4}$")[1], "-",
               str_extract(var_right, "(?<=_)\\d{4}$")[2], ")")
      } else {
        paste0(var_right_title, " (", 
               str_extract(var_right[1], "(?<=_)\\d{4}$"), ")")
      }
    
    labs_xy <- list(labs(
      x = var_right_lab,
      y = paste0(var_left_title, " (\u0394 ", 
                 str_extract(var_left, "(?<=_)\\d{4}$")[1], "-",
                 str_extract(var_left, "(?<=_)\\d{4}$")[2], ")")))
  } 
  
  if (graph_type == "date") labs_xy <- list(labs(
    x = NULL, y = paste0(var_left_title, " (", paste(
      str_extract(var_left, "(?<=_)\\d{4}$"), collapse = "-"), ")")))
  
  return(labs_xy)
  
}
