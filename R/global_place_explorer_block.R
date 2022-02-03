#### PLACE EXPLORER BLOCK FUNCTION #############################################

#' @param df A character string representing the underlying data set to be 
#' loaded. The current options are borough, CT and DA.
#' @param selected_var A character string representing the variable to show 
#' information for.
#' @param select_id A character string giving the ID of a row in the input data 
#' frame (`df`) which has been selected.
#' @return A list of info text and graph to be used in place explorer module

place_explorer_block_text <- function(df, selected_var, select_id) {
  
  ## Setup -------------------------------------------------------------------
  
  selected_var_q3 <- paste0(selected_var, "_q3")
  
  var_dates <- 
    tidyr::unnest(filter(variables, var_code == selected_var), dates)$dates

    if (!(length(var_dates) == 1 && is.na(var_dates))) {
      selected_var <- paste0(selected_var, "_", var_dates[length(var_dates)])
      selected_var_q3 <- paste0(selected_var_q3, "_", var_dates[length(var_dates)])
    }
  
  # Get dataframe from df characters tring
  data <- get(df)
  
  
  ## Retrieve text and graph -------------------------------------------------
  
  var_type <- get_var_type(data, selected_var, " ", df, select_id)
  
  data <- 
    data |> 
    st_drop_geometry() |> 
    select(ID:households, all_of(selected_var), all_of(selected_var_q3)) |> 
    rename(ID = ID,
           var_left = all_of(selected_var),
           var_left_q3 = all_of(selected_var_q3)) |> 
    select(ID:households, var_left, var_left_q3)
  
  info_table(data, var_type, selected_var, " ", df, select_id) |> 
    str_remove("<strong>.*</strong>") |> 
    str_remove("population of .* and a ")
    
}


place_explorer_block_graph <- function(df, selected_var, select_id) {
  
  ## Setup -------------------------------------------------------------------
  
  selected_var_q3 <- paste0(selected_var, "_q3")
  
  var_dates <- 
    tidyr::unnest(filter(variables, var_code == selected_var), dates)$dates
  
  if (!(length(var_dates) == 1 && is.na(var_dates))) {
    selected_var <- paste0(selected_var, "_", var_dates[length(var_dates)])
    selected_var_q3 <- paste0(selected_var_q3, "_", var_dates[length(var_dates)])
  }
  
  # Get dataframe from df characters tring
  data <- get(df)
  
  
  ## Retrieve text and graph -------------------------------------------------
  
  var_type <- get_var_type(data, selected_var, " ", df, select_id)
  
  data <- 
    data |> 
    st_drop_geometry() |> 
    select(ID:households, all_of(selected_var), all_of(selected_var_q3)) |> 
    rename(ID = ID,
           var_left = all_of(selected_var),
           var_left_q3 = all_of(selected_var_q3)) |> 
    select(ID:households, var_left, var_left_q3)
  
  
  explore_graph(data, var_type, selected_var, " ", df, select_id, 
                build_str_as_DA = TRUE, plot_type = "auto")
}
