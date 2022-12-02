#### DATA EXPORT MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A character string representing the left and 
#' right variables to be mapped and analyzed. 
#' @param df A character string representing the dataframe from which the data
#' originates
#' @return A list containing a dataframe with the data ready to get exported,
#' and other information related to the module or data.


data_export <- function(id, data, var_left, var_right = " ", df = NULL) {
  
  # Drop columns
  drop <- c(str_subset(names(data), "q3|q5"), "group")
  dat <- data[setdiff(names(data), drop)]
  
  # var_code
  var_left_code <- unique(str_remove(var_left, "_\\d{4}$"))
  var_right_code <- unique(str_remove(var_right, "_\\d{4}$"))
  
  # Rename columns
  if (length(var_left) == 2) {
    names(dat)[names(dat) == "var_left_1"] <- var_left[1]
    names(dat)[names(dat) == "var_left_2"] <- var_left[2]
    names(dat)[names(dat) == "var_left"] <- paste0(var_left_code, "_variation")
    
  } else {
    names(dat)[names(dat) == "var_left"] <- var_left
  }
  
  if (length(var_right) == 2) {
    names(dat)[names(dat) == "var_right_1"] <- var_right[1]
    names(dat)[names(dat) == "var_right_2"] <- var_right[2]
    names(dat)[names(dat) == "var_right"] <- paste0(var_right_code, "_variation")
  } else {
    names(dat)[names(dat) == "var_right"] <- var_right
  }
  
  # Error checking
  invisible(lapply(c(var_left_code, var_right_code), \(x) {
    if (!x %in% variables$var_code && x != " ") 
      stop(paste0("Variable `", x, "` is not in the 'variables' dataframe.",
                  " It cannot be described for transparency and export."))
  }))
  
  # Origin data
  data_origin <- if (!is.null(df)) {
    if (is_scale_in_df("building", df)) paste0(gsub("building", "DA", df)) else 
      df}

  # Return list
  return(list(id = id, 
              data = dat,
              var_left = var_left,
              var_right = var_right,
              var_left_code = var_left_code,
              var_right_code = var_right_code,
              df = df,
              data_origin = data_origin))
  
}
