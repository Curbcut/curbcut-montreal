#### GET DATA ##################################################################

get_data <- function(df, var_left, var_right, island = FALSE, point_df = NULL,
                     build_str_as_DA = TRUE) {
  
  # Return raw df or NULL if df isn't whitelisted
  if (!df %in% c("borough", "CT", "DA", "building", "grid")) return(get0(df))

  # Get data type
  data_type <- get_data_type(df, var_left, var_right, build_str_as_DA)

  # Are var_left and var_right the same column?
  if (all(var_left == var_right)) {
    warning(glue("`{var_left}` (var_left) and `{var_right}` (var_right)",
                 " are the same. Returning NULL."))
    return(NULL)
  }
  
  
  ## Get data table ------------------------------------------------------------
  
  data <- tryCatch(
    get_data_table(df, var_left, var_right, data_type, point_df),
    error = function(e) {
      warning(glue("get_data() failed with `{df}` (df), `{var_left}` ",
                   "(var_left) and `{var_right}` (var_right). Returning NULL."))
      return(NULL)
    })
  
  if (is.null(data)) return(NULL)
  
  ## Filter to island ----------------------------------------------------------
  
  if (island && df %in% c("borough", "CT", "DA", "grid", "street", "building"))
    data <- data[data$CSDUID %in% island_CSDUID,]
  
  
  # Return output ----------------------------------------------------------
  
  return(data)
  
}
