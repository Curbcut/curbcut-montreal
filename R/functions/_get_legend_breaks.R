#### GET LEGEND BREAKS #########################################################

get_legend_breaks <- function(r = r, data, var_left, var_right, df, geo, 
                              data_type, breaks) {
  
  ## Return NULL if no data_type matches ---------------------------------------
  
  break_labs <- NULL
  
  
  ## If manual breaks are supplied, format them --------------------------------

  if (!is.null(breaks)) {
    break_labs <- convert_unit(breaks, var_left, TRUE)
    attr(break_labs, "qual") <- FALSE
    return(break_labs)
  }
  
  
  ## Univariate q5 version -----------------------------------------------------
  
  if (data_type == "q5") {
    
    # Get break labels
    break_labs <- variables$breaks_q5[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
    if (length(break_labs) > 0) break_labs <- 
        break_labs[break_labs$scale == df & break_labs$geo == geo, ]
    
    if (suppressWarnings(!is.null(break_labs$var_name) && 
                         !any(is.na(break_labs$var_name)))) {
      
      qual <- TRUE
      break_labs <- break_labs$var_name_short
      
    } else {
      
      qual <- FALSE
      break_labs <- break_labs$var
      
      # Format break labels
      if (str_detect(var_left, "_pct|_dollar|_count")) {
        break_labs <- convert_unit(break_labs, var_left, TRUE)
      }
    }
    
    # Add qual attribute
    attr(break_labs, "qual") <- qual
    
  }
  
  
  ## Univariate qualitative version --------------------------------------------
  
  if (data_type == "qual") {
    
    # Get break labels
    break_labs <- variables$breaks_q5[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
    if (length(break_labs) > 0) break_labs <- 
        break_labs[break_labs$scale == df & breaks_labs$geo == geo,]
    
    if (suppressWarnings(!is.null(break_labs$var_name) && 
                         !any(is.na(break_labs$var_name)))) {
      break_labs <- sapply(break_labs$var_name_short, 
                           \(x) cc_t(r = r, x),
                           USE.NAMES = FALSE)
    }
  }
  
  
  ## Bivariate version ---------------------------------------------------------
  
  if (data_type == "bivar") {
    
    date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
    date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
    
    # Get breaks
    break_labs_y <- variables$breaks_q3[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_left)))]]
    if (length(break_labs_y) > 0) break_labs_y <- 
      break_labs_y$var[
        (break_labs_y$date == date_left | is.na(break_labs_y$date)) &
          break_labs_y$scale == df & break_labs_y$geo == geo]
    
    break_labs_x <- variables$breaks_q3[[
      which(variables$var_code == unique(sub("_\\d{4}$", "", var_right)))]]
    if (length(break_labs_x) > 0) break_labs_x <- 
      break_labs_x$var[
        (break_labs_x$date == date_right | is.na(break_labs_x$date)) &
          break_labs_x$scale == df & break_labs_x$geo == geo]
    
    # Format breaks
    break_labs_y <- convert_unit(break_labs_y, var_left, TRUE)
    break_labs_x <- convert_unit(break_labs_x, var_right, TRUE)
    
    # Construct result
    break_labs <- list(x = break_labs_x, y = break_labs_y)
    attr(break_labs, "qual") <- FALSE
     
  }
  
  ## Delta x & q3 y version ----------------------------------------------------
  
  if (data_type == "bivar_xdelta_yq3") {
    
    # Get breaks
    break_labs_y <- c(
      min(data$var_left, na.rm = TRUE),
      max(data$var_left[data$var_left_q3 == 1], na.rm = TRUE),
      max(data$var_left[data$var_left_q3 == 2], na.rm = TRUE),
      max(data$var_left, na.rm = TRUE))
    
    break_labs_x <- c(
      min(data$var_right, na.rm = TRUE),
      max(data$var_right[data$var_right_q3 == 1], na.rm = TRUE),
      max(data$var_right[data$var_right_q3 == 2], na.rm = TRUE),
      max(data$var_right, na.rm = TRUE))
    
    # Format breaks
    break_labs_y <- convert_unit(break_labs_y, var_name = "_pct", TRUE)
    break_labs_x <- convert_unit(break_labs_x, var_name = var_right, TRUE)
    
    # Construct result
    break_labs <- list(x = break_labs_x, y = break_labs_y)
    attr(break_labs, "qual") <- FALSE
    
  }
  
  
  
  ## Delta bivariate version ---------------------------------------------------
  
  if (data_type == "delta_bivar") {
    
    # Get breaks
    break_labs_y <- c(
      min(data$var_left, na.rm = TRUE),
      max(data$var_left[data$var_left_q3 == 1], na.rm = TRUE),
      max(data$var_left[data$var_left_q3 == 2], na.rm = TRUE),
      max(data$var_left, na.rm = TRUE))
    
    break_labs_x <- c(
      min(data$var_right, na.rm = TRUE),
      max(data$var_right[data$var_right_q3 == 1], na.rm = TRUE),
      max(data$var_right[data$var_right_q3 == 2], na.rm = TRUE),
      max(data$var_right, na.rm = TRUE))

    # Format breaks
    break_labs_y <- convert_unit(break_labs_y, "_pct", TRUE)
    break_labs_x <- convert_unit(break_labs_x, "_pct", TRUE)
    
    # Construct result
    break_labs <- list(x = break_labs_x, y = break_labs_y)
    attr(break_labs, "qual") <- FALSE
    
  }
  
  ## Univariate q100 version ---------------------------------------------------
  
  if (data_type == "q100") {
    
    # Create break labels
    break_labs <- c(cc_t(r = r, "Low"), sapply(1:9, \(x) NULL),
                    cc_t(r = r, "High"))
    
  }
  
  
  ## Return output -------------------------------------------------------------
  
  return(break_labs)
  
}
