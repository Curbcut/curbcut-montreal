#### GET DATA TYPE #############################################################

get_data_type <- function(df, var_left, var_right) {
  
  # Building special cases -----------------------------------------------------

  if (df == "building" && length(var_right) == 2 && 
      var_right[1] == var_right[2]) return("building_NA")
  if (df == "building" && length(var_left) == 2 && 
      var_left[1] == var_left[2]) return("building_NA")
  
  if (df == "building" && length(var_left) == 1 && var_right[1] == " ")
    return("building_q5")
  
  if (df == "building" && length(var_left) == 1 && length(var_right) == 1 && 
      var_right != " ")
    return("building_bivar")
  
  if (df == "building" && length(var_left) == 2 && var_right[1] == " ")
    return("building_delta")
  
  if (df == "building" && length(var_left) == 2 && length(var_right) == 2)
    return("building_delta_bivar")
  
  
  # General cases --------------------------------------------------------------

  if (df %in% c("heatmap", "point")) return("point")
  if (length(var_right) == 2 && var_right[1] == var_right[2]) return("NA")
  if (length(var_left) == 2 && var_left[1] == var_left[2]) return("NA")
  if (length(var_left) == 1 && var_right[1] == " ") return("q5")
  if (length(var_left) == 1 && length(var_right) == 1 && var_right != " ")
    return("bivar")
  if (length(var_left) == 2 && length(var_right) == 2) return("delta_bivar")
  if (length(var_left) == 2 && var_right[1] == " ") return("delta")
  
  
  # Fall back if no other types are detected -----------------------------------
  
  return("other")
  
}
