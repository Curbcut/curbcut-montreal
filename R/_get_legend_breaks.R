#### GET LEGEND BREAKS #########################################################

get_legend_breaks <- function(data, var_left, var_right, df, data_type) {
  
  # Return NULL if no data_type matches
  break_labels <- NULL

  
  ## Univariate q5 version -----------------------------------------------------
  
  if (data_type == "q5") {
    
    # Get break labels
    break_labels <- 
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(breaks_q5) |> 
      pluck(1) |> 
      filter(scale == df)
    
    if (suppressWarnings(!is.null(break_labels$var_name) && 
                         !any(is.na(break_labels$var_name)))) {
      
      qual <- TRUE
      
      break_labels <- 
        break_labels |> 
        pull(var_name_short)
      
    } else {
      
      qual <- FALSE
      
      break_labels <- break_labels$var
      
      # Format break labels
      if (str_detect(var_left, "_pct|_dollar")) {
        break_labels <- convert_unit(break_labels, var_left, TRUE)
      }
    }
    
    # Add qual attribute
    attr(break_labels, "qual") <- qual
    
  }
  
  
  ## Univariate qualitative version --------------------------------------------
  
  if (data_type == "qual") {
    # TKTK add this when we have a qualitative variable to visualize
  }
  
  
  ## Bivariate version ---------------------------------------------------------
  
  if (data_type == "bivar") {
    
    date_left <- str_extract(var_left, "(?<=_)\\d{4}$")
    date_right <- str_extract(var_right, "(?<=_)\\d{4}$")
    
    # Get breaks
    break_labels_y <- 
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(breaks_q3) |> 
      pluck(1) |> 
      filter(date == date_left | is.na(date)) |> 
      filter(scale == df) |> 
      pull(var)
    
    break_labels_x <- 
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_right))) |> 
      pull(breaks_q3) |> 
      pluck(1) |> 
      filter(date == date_right | is.na(date)) |> 
      filter(scale == df) |> 
      pull(var)
    
    # Format breaks
    break_labels_y <- convert_unit(break_labels_y, var_left, TRUE)
    break_labels_x <- convert_unit(break_labels_x, var_right, TRUE)
    
    # Construct result
    break_labels <- list(x = break_labels_x, y = break_labels_y)
    attr(break_labels, "qual") <- FALSE
     
  }
  
  
  ## Delta version -------------------------------------------------------------
  
  if (data_type == "delta") {
    
    # Get break labels
    break_labels <- 
      variables |> 
      filter(var_code == unique(sub("_\\d{4}$", "", var_left))) |> 
      pull(breaks_q5) |> 
      pluck(1) |> 
      filter(scale == df)
    
    if (suppressWarnings(!is.null(break_labels$var_name) && 
                         !any(is.na(break_labels$var_name)))) {
      
      qual <- TRUE
      
      break_labels <- 
        break_labels |> 
        filter(rank >= 1) |> 
        pull(var_name_short)
      
    } else {
      
      qual <- FALSE
      
      break_labels <- break_labels$var
      
      # Format break labels
      if (str_detect(var_left[1], "_pct|_dollar")) {
        break_labels <- convert_unit(break_labels, var_left[1], TRUE)
      }
    }
    
    # Add qual attribute
    attr(break_labels, "qual") <- qual
    
  }
  
  
  ## Delta bivariate version ---------------------------------------------------
  
  if (data_type == "delta_bivar") {
    
    # Need to build breaks manually
    break_labels_y <- 
      data |> 
      st_drop_geometry() |> 
      summarize(
        ranks = c(min(var_left, na.rm = TRUE),
                  min(var_left[var_left_q3 == 2], na.rm = TRUE),
                  min(var_left[var_left_q3 == 3], na.rm = TRUE),
                  max(var_left, na.rm = TRUE))) |>
      mutate(ranks = if_else(is.infinite(ranks), NA_real_, ranks)) |> 
      pull(ranks) |> 
      convert_unit("_pct", TRUE)

    break_labels_x <- 
      data |> 
      st_drop_geometry() |> 
      summarize(
        ranks = c(min(var_right, na.rm = TRUE),
                  min(var_right[var_right_q3 == 2], na.rm = TRUE),
                  min(var_right[var_right_q3 == 3], na.rm = TRUE),
                  max(var_right, na.rm = TRUE))) |>
      mutate(ranks = if_else(is.infinite(ranks), NA_real_, ranks)) |> 
      pull(ranks) |> 
      convert_unit("_pct", TRUE)
    
    # Construct result
    break_labels <- list(x = break_labels_x, y = break_labels_y)
    attr(break_labels, "qual") <- FALSE
    
  }
    
  ## Return output -------------------------------------------------------------
  
  return(break_labels)
  
}