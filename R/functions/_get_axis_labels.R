#### GET EXPLORE GRAPH AXIS LABELS #############################################

get_axis_labels <- function(graph_type, var_left, var_right) {
  
  ## Get basic axis labels -----------------------------------------------------
  
  var_left_title <- sus_translate(
    variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(var_title))
  
  var_right_title <- sus_translate(
    variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_right))) |> 
      pull(var_title))
  
  
  ## Construct labs_xy based on graph_type -------------------------------------
  
  if (graph_type %in% c("hist", "bar")) labs_xy <- 
    list(labs(x = var_left_title, y = NULL))
  
  if (graph_type == "scatter") labs_xy <- 
    list(labs(x = var_right_title, y = var_left_title))
  
  if (graph_type == "box") labs_xy <- 
    list(labs(x = var_left_title, y = var_right_title))
  
  if (graph_type %in% c("delta", "NAdelta")) labs_xy <- list(labs(
    x = paste0(var_left_title, " (", 
               str_extract(var_left, "(?<=_)\\d{4}$")[1], ")"),
    y = paste0(var_left_title, " (", 
               str_extract(var_left, "(?<=_)\\d{4}$")[2], ")")))
  
  if (graph_type %in% c("deltabivar", "NAdeltabivar")) {
    
    var_left_title <-
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(var_short)
    
    var_right_title <-
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_right))) |> 
      pull(var_short)
    
    labs_xy <- list(labs(
      x = paste0(var_right_title, " (\u0394 ", 
                 str_extract(var_right, "(?<=_)\\d{4}$")[1], "-",
                 str_extract(var_right, "(?<=_)\\d{4}$")[2], ")"),
      y = paste0(var_left_title, " (\u0394 ", 
                 str_extract(var_left, "(?<=_)\\d{4}$")[1], "-",
                 str_extract(var_left, "(?<=_)\\d{4}$")[2], ")")))
  } 
  
  if (graph_type == "date") labs_xy <- list(labs(
    x = NULL, y = paste0(var_left_title, " (", paste(
      str_extract(var_left, "(?<=_)\\d{4}$"), collapse = "-"), ")")))
  
  return(labs_xy)
  
}