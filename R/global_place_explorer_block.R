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
  
  # Attach year in case of census data.
  year <- 
    if (filter(variables, var_code == selected_var)$source == "census") {
      tidyr::unnest(filter(variables, var_code == selected_var), dates) |> 
        filter(dates == max(dates)) |> 
        pull(dates)
    } else return("No information yet.")
  
  # Get dataframe from df characters tring
  data <- get(df)
  
  
  ## Retrieve text and graph -------------------------------------------------
  
  var_type <- get_var_type(data, selected_var, " ", df, select_id)
  
  data <- 
    data |> 
    st_drop_geometry() |> 
    select(ID:households, any_of(starts_with(selected_var)) & ends_with(year)) |> 
    rename(ID = ID,
           var_left = paste0(selected_var, "_", year),
           var_left_q3 = paste0(selected_var, "_q3_", year)) |> 
    select(ID:households, var_left, var_left_q3)
  
  info_table(data, var_type, selected_var, " ", df, select_id)
}


place_explorer_block_graph <- function(df, selected_var, select_id) {
  
  
  ## Setup -------------------------------------------------------------------
  
  # Attach year in case of census data.
  year <- 
    if (filter(variables, var_code == selected_var)$source == "census") {
      tidyr::unnest(filter(variables, var_code == selected_var), dates) |> 
        filter(dates == max(dates)) |> 
        pull(dates)
    } else return(NULL)
  
  # Get dataframe from df characters tring
  data <- get(df)
  
  
  ## Retrieve text and graph -------------------------------------------------
  
  var_type <- get_var_type(data, selected_var, " ", df, select_id)
  
  data <- 
    data |> 
    st_drop_geometry() |> 
    select(ID:households, any_of(starts_with(selected_var)) & ends_with(year)) |> 
    rename(ID = ID,
           var_left = paste0(selected_var, "_", year),
           var_left_q3 = paste0(selected_var, "_q3_", year)) |> 
    select(ID:households, var_left, var_left_q3)
  
  
  explore_graph(data, var_type, selected_var, " ", df, select_id, 
                build_str_as_DA = TRUE, plot_type = "auto")
}