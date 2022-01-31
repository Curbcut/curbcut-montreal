#### GET EXPLORE PLOT TYPE #####################################################

get_plot_type <- function(data, var_type, var_left, var_right, select_id, df) {
  
  # Check arguments
  stopifnot(!is.reactive(data))
  stopifnot(!is.reactive(var_type))
  stopifnot(!is.reactive(var_left))
  stopifnot(!is.reactive(var_right))
  stopifnot(!is.reactive(select_id))
  stopifnot(!is.reactive(df))
  
  # Convenience variables
  var_left_num <- length(unique(data$var_left))
  na_select <- if (var_right[1] == " ") {
    nrow(filter(data, ID == select_id, !is.na(var_left_q3)))
  } else {
    nrow(filter(data, ID == select_id, !is.na(var_left_q3), 
                !is.na(var_right_q3)))
  }
  
  # Get main graph type
  graph_type <- case_when(
    df == "date" ~ "date",
    var_type == "NA_delta" ~ "NAdelta",
    var_type == "NA_delta_bivar" ~ "NAdeltabivar",
    var_right[1] == " " & grepl("_delta", var_type) ~ "delta",
    var_right[1] != " " & grepl("_delta", var_type) ~ "deltabi",
    var_right[1] == " " & var_left_num > 7 ~ "hist",
    var_right[1] == " " & var_left_num <= 7 ~ "bar",
    var_right[1] != " " & var_left_num > 7 ~ "scatter",
    var_right[1] != " " & var_left_num <= 7 ~ "box") |> 
    unique()
    
  # Get selection status
  select_type <- unique(case_when(is.na(select_id) ~ "all", 
                                  na_select == 0 ~ "na",
                                  TRUE ~ "select"))
  
  # Combine into plot_type and return
  plot_type <- paste(graph_type, select_type, sep = "_")
  return(plot_type)
  
}
