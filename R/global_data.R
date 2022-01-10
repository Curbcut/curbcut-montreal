#### DATA MODULE GLOBALS #######################################################

get_data_type <- function(df, var_left, var_right) {
  
  # Building special cases -----------------------------------------------------

  if (df() == "building" && length(var_left()) == 1 && var_right() == " ")
    return("building_q5")
  
  if (df() == "building" && length(var_left()) == 1 && 
      length(var_right()) == 1 && var_right() != " ")
    return("building_bivar")
  
  if (df() == "building" && length(var_left()) == 2 && var_right() == " ")
    return("building_delta")
  
  if (df() == "building" && length(var_left()) == 2 && length(var_right()) == 2)
    return("building_delta_bivar")
  
  
  # General cases --------------------------------------------------------------

  if (var_right() == " " && length(var_left()) == 1) return("q5")
  
  if (length(var_left()) == 1 && length(var_right()) == 1 && var_right() != " ")
    return("bivar")
  
  if (length(var_left()) == 2 && var_right() == " ") return("delta")
  
  if (length(var_left()) == 2 && length(var_right()) == 2) return("delta_bivar")
  
  
  # Fall back if no other types are detected -----------------------------------
  
  return("other")
  
}