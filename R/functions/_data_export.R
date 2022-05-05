#### DATA EXPORT MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A character string representing the left and 
#' right variables to be mapped and analyzed. 
#' @param df A character string representing the dataframe from which the data
#' originates
#' @return A list containing a dataframe with the data ready to get exported,
#' and other information related to the module or data.

data_export <- function(id, data, var_left, var_right, df) {
  
  # Drop columns
  drop <- c(str_subset(names(data), "q3|q5"), "group")
  dat <- data[setdiff(names(data), drop)]
  
  # Rename var_left column
  # if (length(data_left_vars) > 1) {
  #   names(data)[names(data) == "left_var"] <- 
  #     paste0("change_", left_var_name)
  #   names(data)[names(data) %in% data_left_vars] <- var_left()
  # } else {
    names(dat)[names(dat) == "var_left"] <- var_left
  # }
  
    # Rename var_left column
    # if (length(data_left_vars) > 1) {
    #   names(data)[names(data) == "left_var"] <- 
    #     paste0("change_", left_var_name)
    #   names(data)[names(data) %in% data_left_vars] <- var_left()
    # } else {
    names(dat)[names(dat) == "var_right"] <- var_right
    # }
    
    var_left_code <- str_remove(var_left, "_\\d{4}$")
    var_right_code <- str_remove(var_right, "_\\d{4}$")
    
    # Error checking
    invisible(lapply(c(var_left_code, var_right_code), \(x) {
      if (!x %in% variables$var_code && x != " ") 
        stop(paste0("Variable `", x, "` is not in the 'variables' dataframe.",
                    " It cannot be described for transparency and export."))
    }))
    
    # Origin data
    df <- if (df == "building") "DA" else df
    
  # Return list
  return(list(id = id, 
              data = dat,
              var_left = var_left,
              var_right = var_right,
              var_left_code = var_left_code,
              var_right_code = var_right_code,
              data_origin = df))
  
}
