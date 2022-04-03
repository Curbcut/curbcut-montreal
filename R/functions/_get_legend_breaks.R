#### GET LEGEND BREAKS #########################################################

get_legend_breaks <- function(data, var_left, var_right, df, data_type) {
  
  # Return NULL if no data_type matches
  break_labs <- NULL

  
  ## Univariate q5 version -----------------------------------------------------
  
  if (data_type == "q5") {
    
    # Get break labels
    break_labs <- variables$breaks_q5[
      variables$var_code == unique(sub("_\\d{4}$", "", var_left))]
    if (length(break_labs) > 0) break_labs <- 
        break_labs[[1]][break_labs[[1]]$scale == df,]
    
    if (suppressWarnings(!is.null(break_labs$var_name) && 
                         !any(is.na(break_labs$var_name)))) {
      
      qual <- TRUE
      break_labs <- break_labs$var_name_short
      
    } else {
      
      qual <- FALSE
      break_labs <- break_labs$var
      
      # Format break labels
      if (str_detect(var_left, "_pct|_dollar")) {
        break_labs <- convert_unit(break_labs, var_left, TRUE)
      }
    }
    
    # Add qual attribute
    attr(break_labs, "qual") <- qual
    
  }
  
  
  ## Univariate qualitative version --------------------------------------------
  
  if (data_type == "qual") {
    
    # Get break labels
    break_labs <- variables$breaks_q5[
      variables$var_code == unique(sub("_\\d{4}$", "", var_left))]
    if (length(break_labs) > 0) break_labs <- 
        break_labs[[1]][break_labs[[1]]$scale == df,]
    
    if (suppressWarnings(!is.null(break_labs$var_name) && 
                         !any(is.na(break_labs$var_name)))) {
      break_labs <- break_labs$var_name_short
    }
  }
  
  
  ## Bivariate version ---------------------------------------------------------
  
  if (data_type == "bivar") {
    
    date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
    date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
    
    # Get breaks
    break_labs_y <- variables$breaks_q3[
      variables$var_code == unique(sub("_\\d{4}$", "", var_left))]
    if (length(break_labs_y) > 0) break_labs_y <- 
      break_labs_y[[1]]$var[
        (break_labs_y[[1]]$date == date_left | 
           is.na(break_labs_y[[1]]$date)) &
          break_labs_y[[1]]$scale == df]
    
    break_labs_x <- variables$breaks_q3[
      variables$var_code == unique(sub("_\\d{4}$", "", var_right))]
    if (length(break_labs_x) > 0) break_labs_x <- 
      break_labs_x[[1]]$var[
        (break_labs_x[[1]]$date == date_right | 
           is.na(break_labs_x[[1]]$date)) &
          break_labs_x[[1]]$scale == df]
    
    # Format breaks
    break_labs_y <- convert_unit(break_labs_y, var_left, TRUE)
    break_labs_x <- convert_unit(break_labs_x, var_right, TRUE)
    
    # Construct result
    break_labs <- list(x = break_labs_x, y = break_labs_y)
    attr(break_labs, "qual") <- FALSE
     
  }
  
  
  ## Return output -------------------------------------------------------------
  
  return(break_labs)
  
}