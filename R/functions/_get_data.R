#### GET DATA ##################################################################

get_data <- function(geo, df, var_left, var_right, 
                     point_df = NULL, build_str_as_DA = TRUE) {
  
  # Return raw df or NULL if df isn't whitelisted
  if (!is_scale_in_df(c(all_choropleth, "grid"), df)) return(get0(df))

  # Get data type
  data_type <- get_data_type(df, var_left, var_right, build_str_as_DA)

  # Are var_left and var_right the same column?
  if (all(var_left == var_right)) {
    if (var_left != " ")
    warning(glue("`{var_left}` (var_left) and `{var_right}` (var_right)",
                 " are the same. Returning NULL."))
    return(NULL)
  }
  
  ## Get data table ------------------------------------------------------------

  data <- tryCatch(
    get_data_table(df, geo, var_left, var_right, data_type, point_df),
    error = function(e) {
      warning(glue("get_data() failed with `{df}` (df), `{var_left}` ",
                   "(var_left) and `{var_right}` (var_right). Returning NULL."))
      return(data.frame())
    })
  
  if (is.null(data)) return(NULL)
  
  
  # Return output ----------------------------------------------------------
  
  return(data)
  
}
