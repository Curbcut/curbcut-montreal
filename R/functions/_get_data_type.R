#### GET DATA TYPE #############################################################

get_data_type <- function(df, var_left, var_right, build_str_as_DA = TRUE) {
  
  # Building special cases -----------------------------------------------------

  if (build_str_as_DA && df == "building" && length(var_right) == 2 && 
      var_right[1] == var_right[2]) return("building_NA_delta_bivar")
  if (build_str_as_DA && df == "building" && length(var_left) == 2 && 
      var_left[1] == var_left[2]) return("building_NA_delta")
  
  if (build_str_as_DA && df == "building" && length(var_left) == 1 
      && var_right[1] == " ") return("building_q5")
  
  if (build_str_as_DA && df == "building" && length(var_left) == 1 && 
      length(var_right) == 1 && var_right != " ") return("building_bivar")
  
  if (build_str_as_DA && df == "building" && length(var_left) == 2 && 
      var_right[1] == " ") return("building_delta")
  
  if (build_str_as_DA && df == "building" && length(var_left) == 2 && 
      length(var_right) == 2) return("building_delta_bivar")
  
  
  # General cases --------------------------------------------------------------

  if (df %in% c("heatmap", "point")) return("point")
  if (length(var_right) == 2 && var_right[1] == var_right[2]) return(
    "NA_delta_bivar")
  if (length(var_left) == 2 && var_left[1] == var_left[2]) return("NA_delta")
  if (length(var_left) == 1 && var_right[1] == " ") return("q5")
  if (length(var_left) == 1 && length(var_right) == 1 && var_right != " ")
    return("bivar")
  if (length(var_left) == 2 && length(var_right) == 2) return("delta_bivar")
  if (length(var_left) == 2 && var_right[1] == " ") return("delta")
  
  
  # Fall back if no other types are detected -----------------------------------
  
  return("other")
  
}
