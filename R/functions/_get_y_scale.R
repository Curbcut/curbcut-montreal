#### GET EXPLORE GRAPH Y SCALE #################################################

get_y_scale <- function(graph_type, data, var_type, var_left, var_right) {
  
  ## Get scale type ------------------------------------------------------------
  
  scale_type <- if (graph_type == "date") {
    "date"
  } else if (graph_type %in% c("deltabivar", "NAdeltabivar")) {
    "deltabivar"
  } else if (graph_type %in% c("delta", "NAdelta") && 
             str_detect(var_left[1], "_pct")) {
    "delta_pct"
  } else if (graph_type %in% c("delta", "NAdelta") &&
             str_detect(var_left[1], "_dollar")) {
    "delta_dollar"
  } else if (graph_type %in% c("delta", "NAdelta")) {
    "delta"
  } else if (graph_type == "box" && str_detect(var_right[1], "_pct")) {
    "box_pct"
  } else if (graph_type == "box" && str_detect(var_right[1], "_dollar")) {
    "box_dollar"
  } else if (graph_type == "box") {
    "box"
  } else if (graph_type == "scatter" && str_detect(var_left[1], "_pct")) {
    "scatter_pct"
  } else if (graph_type == "scatter" && str_detect(var_left[1], "_dollar")) {
    "scatter_dollar"
  } else if (graph_type %in% c("bar")) {
    "discrete"
  } else if (graph_type == "scatter") {
    "scatter"
  } else if (graph_type == "hist") {
    "hist"
  }
  
  
  ## Compress dollar values ----------------------------------------------------
  
  if (str_detect(scale_type, "dollar")) {
    
    if (str_detect(var_type, "NA")) lab_dl <- scales::label_dollar() else {
      
      if (str_detect(scale_type, "delta_")) {
        min_dig <- data$var_left_2
      } else {
        min_dig <- data$var_left
      }
      
      min_dig <- 
        min_dig |> 
        setdiff(0) |> 
        abs() |> 
        min(na.rm = TRUE) |> 
        log10() |> 
        ceiling()
      
      if (min_dig >= 10) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+09, suffix = "B")  
      } else if (min_dig >= 7) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+06, suffix = "M")  
      } else if (min_dig >= 4) {
        lab_dl <- scales::label_dollar(scale = 1 / 1e+03, suffix = "K")  
      } else lab_dl <- scales::label_dollar()
      
    }
  }
  
  
  ## Get scale -----------------------------------------------------------------
  
  if (scale_type == "date") out <- 
      list(scale_y_continuous(labels = scales::comma))
  
  if (scale_type == "deltabivar") out <- 
      list(scale_y_continuous(labels = scales::percent))
  
  if (scale_type == "delta_pct") out <- 
      list(scale_y_continuous(labels = scales::percent))
  
  if (scale_type == "delta_dollar") out <- 
      list(scale_y_continuous(labels = lab_dl))
  
  if (scale_type == "delta") out <- 
      list(scale_y_continuous(labels = scales::comma))
  
  if (scale_type == "box_pct") out <- 
      list(scale_y_continuous(labels = scales::percent))
  
  if (scale_type == "box_dollar") out <- 
      list(scale_y_continuous(labels = lab_dl))
  
  if (scale_type == "box") out <- 
      list(scale_y_continuous(labels = scales::comma))
  
  if (scale_type == "scatter_pct") out <- 
      list(scale_y_continuous(labels = scales::percent))
  
  if (scale_type == "scatter_dollar") out <- 
      list(scale_y_continuous(labels = lab_dl))
  
  if (scale_type == "scatter") out <- 
      list(scale_y_continuous(labels = scales::comma))
  
  if (scale_type == "discrete") out <- 
      list(scale_y_continuous(labels = scales::label_comma(accuracy = 1)))
  
  if (scale_type == "hist") out <- 
      list(scale_y_continuous(labels = scales::comma))
  
  return(out)
  
}
