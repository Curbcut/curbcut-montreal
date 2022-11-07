#### GET DATA TYPE #############################################################

get_data_type <- function(df, var_left, var_right, build_str_as_DA = TRUE) {
  
  # Building special cases -----------------------------------------------------

  if (build_str_as_DA && is_scale_in_df("building", df) && length(var_right) == 2 && 
      var_right[1] == var_right[2]) return("building_NA_delta_bivar")
  if (build_str_as_DA && is_scale_in_df("building", df) && length(var_left) == 2 && 
      var_left[1] == var_left[2]) return("building_NA_delta")
  
  if (build_str_as_DA && is_scale_in_df("building", df) && length(var_left) == 1 
      && var_right[1] == " ") return("building_q5")
  
  if (build_str_as_DA && is_scale_in_df("building", df) && length(var_left) == 1 && 
      length(var_right) == 1 && var_right != " ") return("building_bivar")
  
  if (build_str_as_DA && is_scale_in_df("building", df) && length(var_left) == 2 && 
      var_right[1] == " ") return("building_delta")
  
  if (build_str_as_DA && is_scale_in_df("building", df) && length(var_left) == 2 && 
      length(var_right) == 2) return("building_delta_bivar")
  
  # General cases --------------------------------------------------------------

  if (is_scale_in_df("raster", df)) return("q100")  
  if (is_scale_in_df(c("heatmap", "point"), df)) return("point")
  if (is_scale_in_df("qual", var_left[1])) return("qual")
  if (!is_scale_in_df(c(all_choropleth, "grid"), df)) return(df)
  if (length(var_right) == 2 && var_right[1] == var_right[2]) return(
    "bivar_xdelta_yq3")
  # if (length(var_left) == 2 && length(unique(var_right)) == 1 && var_right[1] != " ") return(
  #   "bivar_xdelta_yq3")
  if (length(var_left) == 2 && var_left[1] == var_left[2]) return("NA_delta")
  if (length(var_left) == 1 && var_right[1] == " ") return("q5")
  if (length(var_left) == 1 && length(var_right) == 1 && var_right != " ")
    return("bivar")
  if (length(var_left) == 2 && length(var_right) == 2) return("delta_bivar")
  if (length(var_left) == 2 && var_right[1] == " ") return("delta")
  
  
  # Fall back if no other types are detected -----------------------------------

  return("other")
  
}
