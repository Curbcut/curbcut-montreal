#### GET EXPLORE PLOT TYPE #####################################################

get_plot_type <- function(data, var_type, var_left, var_right, select_id, df) {
  
  # Check arguments
  stopifnot(!is.reactive(data))
  stopifnot(!is.reactive(var_type))
  stopifnot(!is.reactive(var_left))
  stopifnot(!is.reactive(var_right))
  stopifnot(!is.reactive(select_id))
  stopifnot(!is.reactive(df))
  
  # If all_na, return all_na
  if (var_type == "all_na") return("all_na")
  
  # Convenience variables
  var_left_num <- length(unique(data$var_left))
  na_select <- if (is.na(select_id)) {
    0L
  } else if (var_right[1] == " ") {
    nrow(data[data$ID == select_id & !is.na(data$var_left_q3),])
  } else {
    nrow(data[data$ID == select_id & !is.na(data$var_left_q3) & 
                !is.na(data$var_right_q3),])
  }
  
  # Get main graph type
  graph_type <-
    if (is_scale_in_df("date", df)) "date" else
      if (var_type == "NA_delta") "NAdelta" else
        if (var_type == "all_na") "all_na" else
          if (var_type == "NA_delta_bivar") "NAdeltabivar" else
            if (var_right[1] == " " && grepl("_delta", var_type)) "delta" else
              if (var_right[1] != " " && 
                  grepl("_delta", var_type)) "deltabivar" else
                    if (var_right[1] == " " && var_left_num > 7) "hist" else
                      if (var_right[1] == " " && var_left_num <= 7) "bar" else
                        if (var_right[1] != " " && var_left_num > 7) "scatter" else
                          if (var_right[1] != " " && var_left_num <= 7) "box"
    
  # Get selection status
  select_type <- 
    if (is.na(select_id)) "all" else
      if (na_select == 0) "na" else
        "select"
    
  # Combine into plot_type and return
  plot_type <- paste(graph_type, select_type, sep = "_")
  return(plot_type)
  
}
