#### GET DATA ##################################################################

get_data <- function(df, var_left, var_right, island = FALSE, point_df = NULL,
                     build_str_as_DA = TRUE) {
  
  ## Setup ---------------------------------------------------------------------
  
  # Error checking
  stopifnot(!is.reactive(df))
  stopifnot(!is.reactive(var_left))
  stopifnot(!is.reactive(var_right))
  stopifnot(!is.reactive(island))
  
  # Get data type
  data_type <- get_data_type(df, var_left, var_right, build_str_as_DA)

  # Are var_left and var_right the same column?
  if (all(var_left == var_right)) {
    stop("`var_left` and `var_right` are the same.")
  }
  
  
  ## Get data table ------------------------------------------------------------
  
  data <- get_data_table(df, var_left, var_right, data_type, point_df)
  
  
  ## Filter to island ----------------------------------------------------------
  
  if (island && df %in% c("borough", "CT", "DA", "grid", "street", "building"))
    data <- data[data$CSDUID %in% island_CSDUID,]
  
  
  # Return output ----------------------------------------------------------
  
  return(data)
  
}
